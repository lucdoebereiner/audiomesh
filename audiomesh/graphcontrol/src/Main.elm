module Main exposing (Msg(..), main, update, view)

--import Html exposing (Html, button, div, span, text)

import Api
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
import Parameters exposing (Mapping(..), Parameter)
import ProcessGraph exposing (..)
import Task
import Utils exposing (..)



-- TODO
-- set link strength
-- show connections when node select and be able to delete/connect
-- creation of new element
-- export graph/load graph
-- poll
-- scope


type alias Model =
    { graph : Maybe UGenGraph
    , outputs : List Int
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
    , fileName : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing [] Nothing Nothing 0.4 1.0 "" "" "" "" BLPF "" "" "" "graph"
    , Api.getGraph GotGraph
    )


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.none


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
    | GotOutputs (Result Http.Error (List Int))
    | Randomize
    | Randomized (Result Http.Error ())
    | UpdatedGraph (Result Http.Error ())
    | SelectNode Graph.NodeId
    | SelectEdge Int
    | DeleteNode Int
    | SetOutput Int Int
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
    | SetProcessParameter Graph.NodeId Int Float
    | SetEdgeWeight Int Float
    | MulAllEdgeWeights Float
    | ConnectLeastConnected
    | DisconnectMostConnected
    | DownloadGraph
    | GotDownloadGraph (Result Http.Error String)
    | SetFileName String
    | GraphJsonLoaded String
    | LoadGraph
    | FileSelected File


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
                    ( { model | graph = Just (mkGraph g) }, Api.getOutputs GotOutputs )

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

        GotOutputs res ->
            case res of
                Ok l ->
                    ( { model | outputs = l }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GetGraph ->
            ( model, Api.getGraph GotGraph )

        DownloadGraph ->
            ( model, Api.getGraphForDownload GotDownloadGraph )

        Randomize ->
            ( model, Api.randomize Randomized )

        Randomized _ ->
            ( model, Api.getGraph GotGraph )

        SelectNode id ->
            -- let
            --     n =
            --         Maybe.andThen (Graph.get id) model.graph
            --             |> Maybe.map .node
            -- in
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

        SetOutput id out ->
            ( model, Api.setOutput UpdatedGraph id out )

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


displayNode : List Int -> Graph.Node UGen -> Element Msg
displayNode outputs n =
    column [ width fill ]
        [ row [ spacing 10 ]
            ([ text (String.fromInt n.id ++ " " ++ ugenLabel n.label) ]
                ++ (if not (List.member n.id outputs) then
                        simpleButton "Delete" (DeleteNode n.id)
                            :: (List.map
                                    (\out ->
                                        simpleButton
                                            ("As output " ++ String.fromInt out)
                                            (SetOutput n.id out)
                                    )
                                <|
                                    List.range 0 (List.length outputs - 1)
                               )

                    else
                        []
                   )
            )
        , row [ spacing 10 ] <|
            List.map
                (\p -> slider (SetProcessParameter n.id p.idx) p)
                (processParameters
                    n.label.process
                )
        ]


slider : (Float -> msg) -> Parameter -> Element msg
slider msg par =
    Input.slider
        [ width (px 300)
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
        , label = Input.labelAbove [] (text par.name)
        , min = 0.0
        , max = 1.0
        , value = Parameters.unmapped par par.value
        , thumb = Input.defaultThumb
        , step = Just 0.0001
        }


volumeSlider : Float -> Element Msg
volumeSlider v =
    slider SetVolume (Parameter -1 v Exp "Output volume" 0.0001 1.0)


edgesSlider : Float -> Element Msg
edgesSlider e =
    slider MulAllEdgeWeights (Parameter -1 e Exp "Edge fac" 0.01 10.0)


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


sinOscInput : String -> Element Msg
sinOscInput v =
    row [ spacing 5, Border.solid, Border.width 1 ]
        [ Input.text [ width (px 80) ]
            { onChange = SetSinOscFreq
            , text = v
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "Freq")
            }
        , Maybe.withDefault none <|
            Maybe.map (\i -> addProcess "SinOsc" (SinOsc { freq = i })) (String.toFloat v)
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
    row [ width fill, spacing 10 ]
        [ addProcess "Add" Add
        , addProcess "Mul" Mul
        , addProcess "Ring" Ring
        , addProcess "Gauss" Gauss
        , addProcess "RMS" RMS
        , addProcess "BitNeg" BitNeg

        -- , addProcess "BitOr" BitOr
        -- , addProcess "BitXOr" BitXOr
        -- , addProcess "BitAnd" BitAnd
        , delayInput m.delayLength
        , sinInput m.sinMul
        , sinOscInput m.sinOscFreq
        , linconInput m.linConA m.linConB
        , filterInput m.filterType m.filterFreq m.filterQ
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
                    (Parameter -1 l.label.strength Exp "weight" 0.0001 2.0)
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
                , simpleButton "Discon Most Connected" DisconnectMostConnected
                , simpleButton "Conn Least Connected" ConnectLeastConnected
                , downloadInput model.fileName
                , simpleButton "Load" LoadGraph
                ]
            , row [ spacing 10 ]
                [ volumeSlider model.volume
                , edgesSlider model.edgesFac
                ]
            , showSelectedEdge model
            , processRow model
            , Maybe.withDefault none (Maybe.map (displayNode model.outputs) selectedNode)
            , case model.graph of
                Just g ->
                    html <|
                        DrawGraph.view g
                            (Maybe.map .id selectedNode)
                            SelectNode
                            SelectEdge
                            model.outputs
                            ( 1000, 800 )
                            ( 1600, 1100 )
                            200.0

                _ ->
                    none
            ]
