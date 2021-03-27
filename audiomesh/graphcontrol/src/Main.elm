port module Main exposing (Msg(..), main, update, view)

--import Html exposing (Html, button, div, span, text)

import Api
import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import DrawGraph
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select
import Graph
import Html exposing (Html)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events exposing (onClick)
import Html.Lazy as Lazy
import Http
import IntDict
import Json.Decode as Dec
import Json.Decode.Pipeline exposing (required)
import Maybe.Extra as M
import OutputIndices
import Parameters exposing (Mapping(..), Parameter)
import ProcessGraph exposing (..)
import Task
import Time
import Utils exposing (..)


type alias Model =
    { graph : Maybe UGenGraph
    , drawGraph : Maybe UGenGraph
    , outputIndices : Array Float
    , selectedNode : Maybe Graph.NodeId -- Maybe (Graph.Node UGen)
    , selectedEdge : Maybe Int --(Graph.Edge Link)
    , volume : Float
    , edgesFac : Float
    , delayLength : String
    , sinMul : String
    , linConA : String
    , linConB : String
    , filterType : FilterType
    , filterFreq : String
    , filterQ : String
    , sinOscFreq : String
    , sinOscFreqMul : String
    , pllFac : String
    , resonatorFreqCenter : String
    , resonatorFreqFactor : String
    , resonatorDecay : String
    , fileName : String
    , waitingToConnect : Maybe Graph.NodeId
    , lastPoll : Float
    , compressorThreshold : String
    , compressorRatio : String
    , compressorMakeup : String
    , spikeThreshold : String
    , spikeTConst : String
    , spikeR : String
    , spikeTRest : String
    , kanekoChainN : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing
        Nothing
        (Array.fromList [ 0.0, 0.0 ])
        Nothing
        Nothing
        0.4
        1.0
        ""
        ""
        ""
        ""
        BLPF
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        "graph"
        Nothing
        0.0
        "0.4"
        "4"
        "2"
        "0.2"
        "0.0001"
        "5"
        "20000"
        "100"
    , Api.getGraph GotGraph
    )


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Time.every 300 Tick
        , Time.every 500 UpdateDrawGraph
        , midiCC (ReceivedMidi << decodeMidiCC)
        ]



--


type alias MidiCC =
    { channel : Int, controller : Int, value : Float }


port midiCC : (Dec.Value -> msg) -> Sub msg


port midiSend : ( Int, Int ) -> Cmd msg


decodeMidiCC : Dec.Value -> Result Dec.Error MidiCC
decodeMidiCC =
    Dec.decodeValue
        (Dec.succeed (\c co v -> { channel = c, controller = co, value = v })
            |> required "channel" Dec.int
            |> required "controller" Dec.int
            |> required "value" Dec.float
        )


sendMidiProcessState : UGen -> Cmd msg
sendMidiProcessState ugen =
    let
        outputAmp =
            Parameters.explin ugen.output_amp 0.0 2.0 0.0 1.0
                |> round
                << (*) 127
    in
    midiSend ( 4, outputAmp )
        :: List.indexedMap
            (\i p ->
                midiSend
                    ( i + 5
                    , round
                        (Parameters.unmapped p p.value * 127)
                    )
            )
            (processParameters ugen.process)
        |> Cmd.batch


sendMidiEdgeState : ProcessGraph.Link -> Cmd msg
sendMidiEdgeState edge =
    let
        w =
            round (Parameters.explin edge.strength 0.0 5.0 0.0 1.0 * 127)

        d =
            round (Parameters.explin edge.delay.delay 0.0 6.0 0.0 1.0 * 127)
    in
    List.map midiSend [ ( 14, d ), ( 15, w ) ]
        |> Cmd.batch


midiDict : ( Maybe (Graph.Node UGen), Maybe (Graph.Edge Link) ) -> Dict Int (Float -> Msg)
midiDict ( node, edge ) =
    let
        nodeAmp =
            \val ->
                case node of
                    Nothing ->
                        NoMsg

                    Just n ->
                        SetNodeOutputAmp n.id (Parameters.linexp val 0.0 1.0 0.0 2.0)

        processPars =
            case node of
                Nothing ->
                    [ \_ -> NoMsg ]

                Just n ->
                    List.map
                        (\p ->
                            \v ->
                                SetProcessParameter n.id
                                    p.idx
                                    (Parameters.mapped { p | value = v })
                        )
                        (processParameters
                            n.label.process
                        )

        edgeW =
            \v ->
                Maybe.map
                    (\e ->
                        SetEdgeWeight e.label.id
                            (Parameters.linexp v 0.0 1.0 0.0 5.0)
                    )
                    edge
                    |> Maybe.withDefault NoMsg

        edgeD =
            \v ->
                Maybe.map
                    (\e ->
                        SetEdgeDelay e.label.id
                            (Parameters.linexp v 0.0 1.0 0.0 6.0)
                    )
                    edge
                    |> Maybe.withDefault NoMsg
    in
    Dict.fromList <|
        [ ( 29
          , \v ->
                if v > 0.5 then
                    SelectPrevNode

                else
                    NoMsg
          )
        , ( 30
          , \v ->
                if v > 0.5 then
                    SelectNextNode

                else
                    NoMsg
          )
        , ( 31
          , \v ->
                if v > 0.5 then
                    SelectPrevEdge

                else
                    NoMsg
          )
        , ( 32
          , \v ->
                if v > 0.5 then
                    SelectNextEdge

                else
                    NoMsg
          )
        , ( 0
          , \v ->
                SetVolume
                    (Parameters.linexp v 0.0 1.0 0.0 1.0)
          )
        , ( 1
          , \v ->
                SetEdgeFac
                    (Parameters.linexp v 0.0 1.0 0.05 5.0)
          )
        , ( 2, \v -> SetOutputs 0 v )
        , ( 3, \v -> SetOutputs 1 v )
        , ( 4, nodeAmp )
        , ( 14, edgeD )
        , ( 15, edgeW )
        ]
            ++ List.indexedMap (\i p -> ( i + 5, p )) processPars



--


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = GotGraph (Result Http.Error BackendGraph)
    | GetGraph
    | SetOutputs Int Float
    | SetNodeOutputAmp Graph.NodeId Float
    | Randomize
    | RandomCircle
    | Randomized (Result Http.Error ())
    | UpdatedGraph (Result Http.Error ())
    | SelectNode Graph.NodeId
    | SelectEdge Int
    | DeleteNode Int
    | SetVolume Float
    | NoOp (Result Http.Error ())
    | NoMsg
    | AddProcess Process
    | SetDelayLength String
    | SetSinMul String
    | SetLinConA String
    | SetLinConB String
    | SetFilterType FilterType
    | SetFilterFreq String
    | SetFilterQ String
    | SetSinOscFreq String
    | SetSinOscFreqMul String
    | SetCompressorThreshold String
    | SetCompressorRatio String
    | SetCompressorMakeup String
    | SetSpikeThreshold String
    | SetSpikeTConst String
    | SetSpikeR String
    | SetSpikeTRest String
    | SetPLLFac String
    | SetResonatorFreqCenter String
    | SetResonatorFreqFactor String
    | SetResonatorDecay String
    | SetKanekoChainN String
    | SetProcessParameter Graph.NodeId Int Float
    | SetEdgeWeight Int Float
    | SetEdgeDelay Int Float
    | DeleteEdge Int
    | SetEdgeFac Float
    | ConnectLeastConnected
    | DisconnectMostConnected
    | DownloadGraph
    | GotDownloadGraph (Result Http.Error String)
    | SetFileName String
    | GraphJsonLoaded String
    | LoadGraph
    | FileSelected File
    | WaitingToConnect Graph.NodeId
    | Connect Graph.NodeId Graph.NodeId Int
    | Tick Time.Posix
    | GotPoll (Result Http.Error Float)
    | SelectPrevNode
    | SelectNextNode
    | SelectPrevEdge
    | SelectNextEdge
    | UpdateDrawGraph Time.Posix
    | ReceivedMidi (Result Dec.Error MidiCC)


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


getSelectedNode model =
    Maybe.andThen
        (\id ->
            Maybe.andThen (Graph.get id) model.graph
                |> Maybe.map .node
        )
        model.selectedNode


selectNode : Model -> Maybe Graph.NodeId -> ( Model, Cmd msg )
selectNode model nextNode =
    let
        newModel =
            { model | selectedNode = nextNode }

        selectedNode =
            getSelectedNode newModel
    in
    ( newModel
    , Maybe.map (sendMidiProcessState << .label) selectedNode
        |> Maybe.withDefault Cmd.none
    )


selectEdge : Model -> Maybe Int -> ( Model, Cmd msg )
selectEdge model nextEdge =
    let
        newModel =
            { model | selectedEdge = nextEdge }

        selectedEdge =
            getSelectedEdge newModel
    in
    ( newModel
    , Maybe.map (sendMidiEdgeState << .label) selectedEdge
        |> Maybe.withDefault Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNodeOutputAmp id amp ->
            ( { model | graph = Maybe.map (ProcessGraph.updateOutputAmp id amp) model.graph }
            , Api.setNodeOutputAmp NoOp id amp
            )

        ReceivedMidi (Ok m) ->
            let
                selectedNode =
                    getSelectedNode model

                -- _ =
                --     Debug.log "midi and node" ( m, selectedNode )
                selectedEdge =
                    getSelectedEdge model

                ( newModel, cmds ) =
                    Dict.get m.controller (midiDict ( selectedNode, selectedEdge ))
                        |> Maybe.map (\ms -> update (ms m.value) model)
                        |> Maybe.withDefault ( model, Cmd.none )
            in
            ( newModel, cmds )

        ReceivedMidi (Err m) ->
            let
                _ =
                    Debug.log "midi error" m
            in
            ( model, Cmd.none )

        UpdateDrawGraph g ->
            ( { model | drawGraph = model.graph }, Cmd.none )

        SelectPrevNode ->
            let
                nextNode =
                    Maybe.andThen
                        (\g ->
                            ProcessGraph.prevNode
                                g
                                model.selectedNode
                        )
                        model.graph
            in
            selectNode model nextNode

        SelectNextNode ->
            let
                nextNode =
                    Maybe.andThen
                        (\g ->
                            ProcessGraph.nextNode
                                g
                                model.selectedNode
                        )
                        model.graph
            in
            selectNode model nextNode

        SelectPrevEdge ->
            let
                nextEdge =
                    Maybe.andThen (\g -> ProcessGraph.prevEdge g model.selectedEdge) model.graph
            in
            selectEdge model nextEdge

        SelectNextEdge ->
            let
                nextEdge =
                    Maybe.andThen (\g -> ProcessGraph.nextEdge g model.selectedEdge) model.graph
            in
            selectEdge model nextEdge

        SetOutputs channel pos ->
            let
                newIndices =
                    Array.set channel pos model.outputIndices

                specs =
                    Maybe.map (OutputIndices.outputSpecs newIndices) model.graph

                -- _ =
                --     Debug.log "set outputs" specs
            in
            ( { model | outputIndices = newIndices, graph = Maybe.map2 OutputIndices.updateGraphWithSpecs specs model.graph }
            , Maybe.map
                (Api.setOutputs
                    NoOp
                )
                specs
                |> Maybe.withDefault Cmd.none
            )

        Tick _ ->
            ( model
            , Maybe.map (\n -> Api.poll GotPoll n) model.selectedNode
                |> Maybe.withDefault Cmd.none
            )

        GotPoll r ->
            case r of
                Ok p ->
                    ( { model | lastPoll = p }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        WaitingToConnect i ->
            case ( model.waitingToConnect, model.selectedNode ) of
                ( Just w, Just n ) ->
                    if i == n && i == w then
                        ( { model | waitingToConnect = Nothing }, Cmd.none )

                    else
                        ( { model | waitingToConnect = Just i }, Cmd.none )

                _ ->
                    ( { model | waitingToConnect = Just i }, Cmd.none )

        FileSelected f ->
            ( model, readFile f )

        LoadGraph ->
            ( model, File.Select.file [ "application/json" ] FileSelected )

        GraphJsonLoaded g ->
            ( model, Api.postGraph UpdatedGraph g )

        SetFileName s ->
            ( { model | fileName = s }, Cmd.none )

        ConnectLeastConnected ->
            ( model, Api.connectLeastConnected UpdatedGraph )

        DisconnectMostConnected ->
            ( model, Api.disconnectMostConnected UpdatedGraph )

        GotGraph res ->
            case res of
                Ok g ->
                    ( { model | graph = Just (mkGraph g) }, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "problem with graph" res
                    in
                    ( model, Cmd.none )

        GotDownloadGraph res ->
            case res of
                Ok g ->
                    ( model
                    , File.Download.string (model.fileName ++ ".json")
                        "application/json"
                        g
                    )

                _ ->
                    ( model, Cmd.none )

        GetGraph ->
            ( model, Api.getGraph GotGraph )

        DownloadGraph ->
            ( model, Api.getGraphForDownload GotDownloadGraph )

        Randomize ->
            ( model, Api.randomize Randomized )

        RandomCircle ->
            ( model, Api.randomCircle Randomized )

        Randomized _ ->
            ( model, Api.getGraph GotGraph )

        SelectNode id ->
            case model.waitingToConnect of
                Just i ->
                    ( { model | waitingToConnect = Nothing }
                    , Api.postEdge UpdatedGraph i id ( 0, 1.0 )
                    )

                Nothing ->
                    selectNode model (Just id)

        Connect from to idx ->
            ( { model | waitingToConnect = Nothing }
            , Api.postEdge UpdatedGraph from to ( idx, 1.0 )
            )

        SelectEdge id ->
            selectEdge model (Just id)

        DeleteNode id ->
            ( model, Api.deleteNode UpdatedGraph id )

        UpdatedGraph _ ->
            ( model, Api.getGraph GotGraph )

        SetVolume v ->
            ( { model | volume = v }, Api.setVolume NoOp v )

        SetDelayLength s ->
            ( { model | delayLength = s }, Cmd.none )

        SetPLLFac s ->
            ( { model | pllFac = s }, Cmd.none )

        SetSinMul s ->
            ( { model | sinMul = s }, Cmd.none )

        SetLinConA s ->
            ( { model | linConA = s }, Cmd.none )

        SetLinConB s ->
            ( { model | linConB = s }, Cmd.none )

        SetResonatorFreqCenter s ->
            ( { model | resonatorFreqCenter = s }, Cmd.none )

        SetResonatorFreqFactor s ->
            ( { model | resonatorFreqFactor = s }, Cmd.none )

        SetResonatorDecay s ->
            ( { model | resonatorDecay = s }, Cmd.none )

        SetFilterType ft ->
            ( { model | filterType = ft }, Cmd.none )

        SetFilterFreq s ->
            ( { model | filterFreq = s }, Cmd.none )

        SetFilterQ s ->
            ( { model | filterQ = s }, Cmd.none )

        SetSinOscFreq s ->
            ( { model | sinOscFreq = s }, Cmd.none )

        SetSinOscFreqMul s ->
            ( { model | sinOscFreqMul = s }, Cmd.none )

        SetCompressorThreshold s ->
            ( { model | compressorThreshold = s }, Cmd.none )

        SetCompressorRatio s ->
            ( { model | compressorRatio = s }, Cmd.none )

        SetCompressorMakeup s ->
            ( { model | compressorMakeup = s }, Cmd.none )

        SetSpikeThreshold s ->
            ( { model | spikeThreshold = s }, Cmd.none )

        SetSpikeTRest s ->
            ( { model | spikeTRest = s }, Cmd.none )

        SetSpikeR s ->
            ( { model | spikeR = s }, Cmd.none )

        SetSpikeTConst s ->
            ( { model | spikeTConst = s }, Cmd.none )

        SetKanekoChainN n ->
            ( { model | kanekoChainN = n }, Cmd.none )

        NoOp _ ->
            ( model, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )

        AddProcess proc ->
            ( model, Api.addNode UpdatedGraph proc )

        SetProcessParameter nodeId parIdx val ->
            let
                -- _ =
                --     Debug.log "para" val
                newGraph =
                    Maybe.map (updateProcessParameter nodeId ( parIdx, val ))
                        model.graph
            in
            ( { model | graph = newGraph }
            , Api.setParameter NoOp nodeId parIdx val
            )

        --            ( model, Cmd.none )
        SetEdgeWeight edgeId weight ->
            ( { model | graph = Maybe.map (updateEdgeWeight edgeId weight) model.graph }
            , Api.setEdgeWeight NoOp edgeId weight
            )

        SetEdgeDelay edgeId delay ->
            ( { model | graph = Maybe.map (updateEdgeDelay edgeId delay) model.graph }
            , Api.setEdgeDelay NoOp edgeId delay
            )

        DeleteEdge e ->
            ( model, Api.deleteEdge UpdatedGraph e )

        SetEdgeFac f ->
            ( { model | edgesFac = f }, Api.setEdgeFac NoOp f )


simpleButton : String -> msg -> Element msg
simpleButton s msg =
    Input.button [ Border.solid, Border.width 1, padding 10 ]
        { onPress = Just msg
        , label = text s
        }


simpleStateButton : String -> Bool -> msg -> Element msg
simpleStateButton s b msg =
    Input.button
        ([ Border.solid, Border.width 1, padding 10 ]
            ++ (if b then
                    [ Background.color (rgb 0.6 0.6 0.6) ]

                else
                    []
               )
        )
        { onPress = Just msg
        , label = text s
        }



-- todo waiting to connect button


displayNode : Float -> Maybe Graph.NodeId -> Graph.Node UGen -> Element Msg
displayNode polled waiting n =
    el [ padding 10, Border.width 1, Border.solid ] <|
        column [ width fill, height fill, spacing 20 ]
            [ row [ spacing 20, width fill ]
                ([ el [ width (px 400) ] <| text (String.fromInt n.id ++ " " ++ ugenLabel n.label)
                 , simpleStateButton "Connect"
                    (M.isJust waiting)
                    (WaitingToConnect n.id)
                 , simpleButton "Delete" (DeleteNode n.id)
                 , el
                    [ width (px 60)
                    , padding 5
                    , Border.solid
                    , Border.width 1
                    ]
                   <|
                    text <|
                        floatString polled
                 , slider (SetNodeOutputAmp n.id)
                    (Parameter -1
                        n.label.output_amp
                        Exp
                        "OutputAmp"
                        0.0
                        2.0
                    )
                 ]
                    ++ List.map
                        (\( out_i, amp ) ->
                            text <|
                                " out: "
                                    ++ String.fromInt out_i
                                    ++ " amp:"
                                    ++ floatString amp
                        )
                        (IntDict.toList n.label.output_sends)
                )
            , row [ spacing 20, width fill ] <|
                List.map
                    (\p -> slider (SetProcessParameter n.id p.idx) p)
                    (processParameters
                        n.label.process
                    )
            ]


slider : (Float -> Msg) -> Parameter -> Element Msg
slider msg par =
    column [ width (px 400) ]
        [ text (par.name ++ " " ++ floatString par.value)
        , el [ width (px 400), height (px 30) ] <|
            html <|
                Html.input
                    [ Attr.type_ "range"
                    , Attr.min "0.0"
                    , Attr.max "1.0"
                    , Attr.class "slider"
                    , Attr.step "0.004"
                    , Attr.value <| String.fromFloat (Parameters.unmapped par par.value)
                    , Events.onInput
                        (\v ->
                            case String.toFloat v of
                                Just val ->
                                    msg (Parameters.mapped { par | value = val })

                                Nothing ->
                                    NoMsg
                        )
                    ]
                    []
        ]



-- slider : (Float -> msg) -> Parameter -> Element msg
-- slider msg par =
--     Input.slider
--         [ width (px 400)
--         , height (px 30)
--         , behindContent
--             (el
--                 [ width fill
--                 , height (px 2)
--                 , centerY
--                 , Background.color (rgb 0.5 0.5 0.5)
--                 , Border.rounded 2
--                 ]
--                 none
--             )
--         ]
--         { onChange = \v -> msg (Parameters.mapped { par | value = v })
--         , label = Input.labelAbove [ width (px 400) ] (text (par.name ++ " " ++ floatString par.value))
--         , min = 0.0
--         , max = 1.0
--         , value = Parameters.unmapped par par.value
--         , thumb = Input.defaultThumb
--         , step = Just 0.01
--         }


volumeSlider : Float -> Element Msg
volumeSlider v =
    slider SetVolume (Parameter -1 v Exp "Output volume" 0.0 1.0)


outputSlider : Int -> Float -> Element Msg
outputSlider channel v =
    slider (SetOutputs channel) (Parameter -1 v Lin ("Output " ++ String.fromInt channel) 0.0 1.0)


edgesSlider : Float -> Element Msg
edgesSlider e =
    slider SetEdgeFac (Parameter -1 e Exp "Edge fac" 0.03 5.0)


addProcess : String -> Process -> Element Msg
addProcess name proc =
    simpleButton name (AddProcess proc)


delayInput : String -> Element Msg
delayInput v =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetDelayLength
            , text = v
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Delay")
            }
        , Maybe.withDefault none <|
            Maybe.map (\i -> addProcess "Delay" (Delay { length = i })) (String.toInt v)
        ]


pllInput : String -> Element Msg
pllInput v =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetPLLFac
            , text = v
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Fac")
            }
        , Maybe.withDefault none <|
            Maybe.map (\f -> addProcess "PLL" (PLL { factor = f })) (String.toFloat v)
        ]


kanekoChainInput : String -> Element Msg
kanekoChainInput v =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetKanekoChainN
            , text = v
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "N")
            }
        , Maybe.withDefault none <|
            Maybe.map (\n -> addProcess "KanekoChain" (KanekoChain { n = n, a = 1.5, e = 0.3 })) (String.toInt v)
        ]


sinOscInput : String -> String -> Element Msg
sinOscInput fr frm =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetSinOscFreq
            , text = fr
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Freq")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetSinOscFreqMul
            , text = frm
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Freq Mul")
            }
        , Maybe.withDefault none <|
            Maybe.map2 (\ff fmf -> addProcess "SinOsc" (SinOsc { freq = ff, freq_mul = fmf })) (String.toFloat fr) (String.toFloat frm)
        ]


sinInput : String -> Element Msg
sinInput v =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetSinMul
            , text = v
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "SinPhMul")
            }
        , Maybe.withDefault none <|
            Maybe.map (\i -> addProcess "Sin" (Sin { mul = i })) (String.toFloat v)
        ]


linconInput : String -> String -> Element Msg
linconInput a b =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetLinConA
            , text = a
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "A")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetLinConB
            , text = b
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "B")
            }
        , Maybe.withDefault none <|
            Maybe.map2 (\af bf -> addProcess "LinCon" (LinCon { linconA = af, linconB = bf }))
                (String.toFloat a)
                (String.toFloat b)
        ]


resonatorInput : String -> String -> String -> Element Msg
resonatorInput c f d =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetResonatorFreqCenter
            , text = c
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Center")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetResonatorFreqFactor
            , text = f
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Fac")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetResonatorDecay
            , text = d
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Decay")
            }
        , Maybe.withDefault none <|
            Maybe.map3
                (\cf ff df ->
                    addProcess "Resonator"
                        (Resonator
                            { freqCenter = cf
                            , freqFactor = ff
                            , decay = df
                            }
                        )
                )
                (String.toFloat c)
                (String.toFloat f)
                (String.toFloat d)
        ]


compressorInput : String -> String -> String -> Element Msg
compressorInput t r m =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetCompressorThreshold
            , text = t
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Threshold")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetCompressorRatio
            , text = r
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Ratio")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetCompressorMakeup
            , text = m
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Makeup")
            }
        , Maybe.withDefault none <|
            Maybe.map3
                (\tf rf mf ->
                    addProcess "Compressor"
                        (Compressor { threshold = tf, ratio = rf, makeup = mf })
                )
                (String.toFloat t)
                (String.toFloat r)
                (String.toFloat m)
        ]


spikeInput : String -> String -> String -> String -> Element Msg
spikeInput t c r rest =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetSpikeThreshold
            , text = t
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Threshold")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetSpikeTConst
            , text = c
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "TConst")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetSpikeTConst
            , text = r
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "TConst")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetSpikeTRest
            , text = rest
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "TRest")
            }
        , Maybe.withDefault none <|
            Maybe.map4
                (\tf cf rf ri ->
                    addProcess "Spike"
                        (Spike { threshold = tf, tConst = cf, r = rf, tRest = ri })
                )
                (String.toFloat t)
                (String.toFloat c)
                (String.toFloat r)
                (String.toInt rest)
        ]


filterInput : FilterType -> String -> String -> Element Msg
filterInput ft fr q =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.radio
            [ padding 10
            , spacing 10
            ]
            { onChange = SetFilterType
            , selected = Just ft
            , label = Input.labelAbove [] (text "Type")
            , options =
                [ Input.option BLPF (text "LP")
                , Input.option BHPF (text "HP")
                , Input.option BBPF (text "BP")
                ]
            }
        , Input.text [ width (px 80) ]
            { onChange = SetFilterFreq
            , text = fr
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Freq")
            }
        , Input.text [ width (px 80) ]
            { onChange = SetFilterQ
            , text = q
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Q")
            }
        , Maybe.withDefault none <|
            Maybe.map2
                (\frF qF ->
                    addProcess "Filter"
                        (Filter { filterType = ft, freq = frF, q = qF })
                )
                (String.toFloat fr)
                (String.toFloat q)
        ]


processRow : Model -> Element Msg
processRow m =
    wrappedRow [ width fill, spacing 10 ]
        [ addProcess "Add" Add
        , addProcess "Mul" Mul
        , addProcess "Softclip" Softclip
        , addProcess "Ring" Ring
        , addProcess "Gauss" Gauss
        , addProcess "RMS" RMS
        , addProcess "Ducking" Ducking
        , addProcess "EnvFollow" EnvFollow
        , addProcess "Env"
            (Env
                { min_target = 0.001
                , max_target = 0.6
                , max_n = 4.0
                , sustain_fac = 1.0
                , rest_fac = 0.1
                }
            )
        , addProcess "VarDelay" (VarDelay { delay = 0.0, maxdelay = 10.0 })
        , addProcess "GateIfGreater" GateIfGreater
        , addProcess "GateDecision"
            (GateDecision
                { min_dur_on = 1.0
                , max_dur_on = 3.0
                , min_dur_off = 3.0
                , max_dur_off = 6.0
                }
            )
        , addProcess "Fold"
            (Fold
                { threshold = 1.0
                , mul = 1.0
                , add = 0.0
                }
            )
        , addProcess "BitNeg" BitNeg
        , addProcess "BitOr" BitOr

        --        , addProcess "BitXOr" BitXOr
        , addProcess "BitAnd" BitAnd
        , addProcess "SoundIn0" (SoundIn { index = 0, factor = 1.0 })
        , addProcess "SoundIn1" (SoundIn { index = 1, factor = 1.0 })
        , addProcess "VanDerPol" (VanDerPol { e = 2, frac = 0.03, a = 1.0 })
        , addProcess "Duffing" (Duffing { e = 0.2, frac = 0.03, a = 0.5 })
        , addProcess "Kaneko" (Kaneko { e = 0.4, a = 1.5 })
        , kanekoChainInput m.kanekoChainN
        , delayInput m.delayLength
        , pllInput m.pllFac
        , sinInput m.sinMul
        , sinOscInput m.sinOscFreq m.sinOscFreqMul
        , linconInput m.linConA m.linConB
        , filterInput m.filterType m.filterFreq m.filterQ
        , resonatorInput m.resonatorFreqCenter m.resonatorFreqFactor m.resonatorDecay
        , compressorInput m.compressorThreshold m.compressorRatio m.compressorMakeup
        , spikeInput m.spikeThreshold m.spikeTConst m.spikeR m.spikeTRest
        ]


downloadInput : String -> Element Msg
downloadInput s =
    row [ spacing 10 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetFileName
            , text = s
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Name")
            }
        , simpleButton "Download" DownloadGraph
        ]


readFile : File -> Cmd Msg
readFile file =
    Task.perform GraphJsonLoaded (File.toString file)


getSelectedEdge model =
    Maybe.andThen
        (\id ->
            Maybe.map Graph.edges model.graph
                |> Maybe.andThen (find (\ed -> ed.label.id == id))
        )
        model.selectedEdge


showSelectedEdge : Model -> Element Msg
showSelectedEdge model =
    let
        e =
            getSelectedEdge model
    in
    Maybe.map
        (\l ->
            el [ padding 10, Border.width 1, Border.solid ] <|
                row [ spacing 10 ]
                    [ --text ("Link weight: " ++ floatString l.label.strength)
                      slider (SetEdgeDelay l.label.id)
                        (Parameter -1 l.label.delay.delay Exp "Delay" 0.0 6.0)
                    , slider (SetEdgeWeight l.label.id)
                        (Parameter -1 l.label.strength Exp "weight" 0.0 5.0)
                    , simpleButton "Delete" (DeleteEdge l.label.id)
                    ]
        )
        e
        |> Maybe.withDefault none


view : Model -> Html Msg
view model =
    let
        selectedNode =
            getSelectedNode model
    in
    layout
        [ spacing 10
        , width fill
        , height fill
        , Font.size 14
        , Font.family
            [ Font.monospace
            ]
        ]
    <|
        column [ width fill, spacing 10 ]
            [ row [ spacing 10 ]
                [ simpleButton "Randomize" Randomize
                , simpleButton "Random Circle" RandomCircle
                , simpleButton "Discon Most Connected" DisconnectMostConnected
                , simpleButton "Conn Least Connected" ConnectLeastConnected
                , downloadInput model.fileName
                , simpleButton "Load" LoadGraph
                ]
            , row [ spacing 10 ]
                ([ volumeSlider model.volume
                 , edgesSlider model.edgesFac
                 ]
                    ++ List.map (\( i, f ) -> outputSlider i f) (Array.toIndexedList model.outputIndices)
                )
            , processRow model
            , showSelectedEdge model
            , Maybe.withDefault
                none
                (Maybe.map
                    (displayNode
                        model.lastPoll
                        model.waitingToConnect
                    )
                    selectedNode
                )
            , row [ spacing 20 ]
                [ simpleButton "Prev Node" SelectPrevNode
                , simpleButton "Next Node" SelectNextNode
                , simpleButton "Prev Edge" SelectPrevEdge
                , simpleButton "Next Edge" SelectNextEdge
                ]
            , case model.drawGraph of
                Just g ->
                    html <|
                        Lazy.lazy3
                            (\graph n e ->
                                DrawGraph.view graph
                                    n
                                    e
                                    SelectNode
                                    SelectEdge
                                    (Maybe.map Connect model.waitingToConnect)
                                    ( 1800, 1400 )
                                    ( 2600, 2000 )
                                    320.0
                            )
                            g
                            model.selectedNode
                            model.selectedEdge

                _ ->
                    none
            ]
