module Main exposing (Msg(..), main, update, view)

--import Html exposing (Html, button, div, span, text)

import Api
import Array exposing (Array)
import Browser
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
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import IntDict
import Maybe.Extra as M
import OutputIndices
import Parameters exposing (Mapping(..), Parameter)
import ProcessGraph exposing (..)
import Task
import Time
import Utils exposing (..)


type alias Model =
    { graph : Maybe UGenGraph
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing
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
    , Api.getGraph GotGraph
    )


subscriptions : Model -> Sub Msg
subscriptions m =
    Time.every 250 Tick


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
    | Randomize
    | RandomCircle
    | Randomized (Result Http.Error ())
    | UpdatedGraph (Result Http.Error ())
    | SelectNode Graph.NodeId
    | SelectEdge Int
    | DeleteNode Int
      --    | SetOutput Int Int
    | SetVolume Float
    | NoOp (Result Http.Error ())
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
    | SetProcessParameter Graph.NodeId Int Float
    | SetEdgeWeight Int Float
    | DeleteEdge Int
    | MulAllEdgeWeights Float
    | ConnectLeastConnected
    | DisconnectMostConnected
    | DownloadGraph
    | GotDownloadGraph (Result Http.Error String)
    | SetFileName String
    | GraphJsonLoaded String
    | LoadGraph
    | FileSelected File
    | WaitingToConnect Graph.NodeId
    | Tick Time.Posix
    | GotPoll (Result Http.Error Float)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetOutputs channel pos ->
            let
                newIndices =
                    Array.set channel pos model.outputIndices

                specs =
                    Maybe.map (OutputIndices.outputSpecs newIndices) model.graph
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
            let
                _ =
                    Debug.log "got graph" res
            in
            case res of
                Ok g ->
                    ( { model | graph = Just (mkGraph g) }, Cmd.none )

                _ ->
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
            -- todo make weight and input variable
            case model.waitingToConnect of
                Just i ->
                    ( { model | waitingToConnect = Nothing }
                    , Api.postEdge UpdatedGraph i id ( 0, 1.0 )
                    )

                Nothing ->
                    ( { model | selectedNode = Just id }, Cmd.none )

        SelectEdge id ->
            -- let
            --     n =
            --         Maybe.map Graph.edges model.graph
            --             |> Maybe.andThen (find (\e -> e.label.id == id))
            -- in
            ( { model | selectedEdge = Just id }, Cmd.none )

        DeleteNode id ->
            ( model, Api.deleteNode UpdatedGraph id )

        UpdatedGraph _ ->
            ( model, Api.getGraph GotGraph )

        SetVolume v ->
            ( { model | volume = v }, Api.setVolume NoOp v )

        SetDelayLength s ->
            ( { model | delayLength = s }, Cmd.none )

        SetSinMul s ->
            ( { model | sinMul = s }, Cmd.none )

        SetLinConA s ->
            ( { model | linConA = s }, Cmd.none )

        SetLinConB s ->
            ( { model | linConB = s }, Cmd.none )

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

        NoOp _ ->
            ( model, Cmd.none )

        AddProcess proc ->
            ( model, Api.addNode UpdatedGraph proc )

        SetProcessParameter nodeId parIdx val ->
            let
                newGraph =
                    Maybe.map (updateProcessParameter nodeId ( parIdx, val ))
                        model.graph
            in
            ( { model | graph = newGraph }
            , Api.setParameter NoOp nodeId parIdx val
            )

        SetEdgeWeight edgeId weight ->
            ( { model | graph = Maybe.map (updateEdge edgeId weight) model.graph }
            , Api.setEdgeWeight NoOp edgeId weight
            )

        DeleteEdge e ->
            ( model, Api.deleteEdge UpdatedGraph e )

        MulAllEdgeWeights f ->
            ( { model
                | -- graph = Maybe.map (mulAllEdges f) model.graph
                  edgesFac = f
              }
            , Maybe.map
                (\g ->
                    Cmd.batch <|
                        List.map
                            (\e ->
                                Api.setEdgeWeight NoOp
                                    e.label.id
                                    (e.label.strength * f)
                            )
                            (Graph.edges g)
                )
                model.graph
                |> Maybe.withDefault Cmd.none
            )


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


slider : (Float -> msg) -> Parameter -> Element msg
slider msg par =
    Input.slider
        [ width (px 400)
        , height (px 30)
        , behindContent
            (el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color (rgb 0.5 0.5 0.5)
                , Border.rounded 2
                ]
                none
            )
        ]
        { onChange = \v -> msg (Parameters.mapped { par | value = v })
        , label = Input.labelAbove [ width (px 400) ] (text (par.name ++ " " ++ floatString par.value))
        , min = 0.0
        , max = 1.0
        , value = Parameters.unmapped par par.value
        , thumb = Input.defaultThumb
        , step = Just 0.00001
        }


volumeSlider : Float -> Element Msg
volumeSlider v =
    slider SetVolume (Parameter -1 v Exp "Output volume" 0.0001 1.0)


outputSlider : Int -> Float -> Element Msg
outputSlider channel v =
    slider (SetOutputs channel) (Parameter -1 v Lin ("Output " ++ String.fromInt channel) 0.0 1.0)


edgesSlider : Float -> Element Msg
edgesSlider e =
    slider MulAllEdgeWeights (Parameter -1 e Exp "Edge fac" 0.05 5.0)


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
        , addProcess "Ring" Ring
        , addProcess "Gauss" Gauss
        , addProcess "RMS" RMS
        , addProcess "BitNeg" BitNeg
        , addProcess "BitOr" BitOr

        --        , addProcess "BitXOr" BitXOr
        , addProcess "BitAnd" BitAnd
        , addProcess "SoundIn0" (SoundIn { index = 0, factor = 1.0 })
        , addProcess "SoundIn1" (SoundIn { index = 1, factor = 1.0 })
        , delayInput m.delayLength
        , sinInput m.sinMul
        , sinOscInput m.sinOscFreq m.sinOscFreqMul
        , linconInput m.linConA m.linConB
        , filterInput m.filterType m.filterFreq m.filterQ
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


showSelectedEdge : Model -> Element Msg
showSelectedEdge model =
    let
        e =
            Maybe.andThen
                (\id ->
                    Maybe.map Graph.edges model.graph
                        |> Maybe.andThen (find (\ed -> ed.label.id == id))
                )
                model.selectedEdge
    in
    Maybe.map
        (\l ->
            row [ spacing 10 ]
                [ text ("Link weight: " ++ floatString l.label.strength)
                , slider (SetEdgeWeight l.label.id)
                    (Parameter -1 l.label.strength Exp "weight" 0.01 2.0)
                , simpleButton "Delete" (DeleteEdge l.label.id)
                ]
        )
        e
        |> Maybe.withDefault none


view : Model -> Html Msg
view model =
    let
        selectedNode =
            Maybe.andThen
                (\id ->
                    Maybe.andThen (Graph.get id) model.graph
                        |> Maybe.map .node
                )
                model.selectedNode
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
            , showSelectedEdge model
            , processRow model
            , Maybe.withDefault none
                (Maybe.map
                    (displayNode
                        model.lastPoll
                        model.waitingToConnect
                     --                        model.outputs
                    )
                    selectedNode
                )
            , case model.graph of
                Just g ->
                    html <|
                        DrawGraph.view g
                            (Maybe.map .id selectedNode)
                            SelectNode
                            SelectEdge
                            []
                            ( 2000, 1600 )
                            ( 3000, 2400 )
                            150.0

                _ ->
                    none
            ]
