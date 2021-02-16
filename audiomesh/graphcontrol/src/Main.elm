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
import Graph
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import ProcessGraph exposing (..)



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
    , selectedNode : Maybe (Graph.Node UGen)
    , selectedEdge : Maybe (Graph.Edge Link)
    , volume : Float
    , delayLength : String
    , sinMul : String
    , linConA : String
    , linConB : String
    , filterType : FilterType
    , filterFreq : String
    , filterQ : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing [] Nothing Nothing 1.0 "" "" "" "" BLPF "" ""
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
    | SelectNode Int
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
        GotGraph res ->
            case res of
                Ok g ->
                    ( { model | graph = Just (mkGraph g) }, Api.getOutputs GotOutputs )

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

        Randomize ->
            ( model, Api.randomize Randomized )

        Randomized _ ->
            ( model, Api.getGraph GotGraph )

        SelectNode id ->
            let
                n =
                    Maybe.andThen (Graph.get id) model.graph
                        |> Maybe.map .node
            in
            ( { model | selectedNode = n }, Cmd.none )

        SelectEdge id ->
            let
                n =
                    Maybe.map Graph.edges model.graph
                        |> Maybe.andThen (find (\e -> e.label.id == id))
            in
            ( { model | selectedEdge = n }, Cmd.none )

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

        NoOp _ ->
            ( model, Cmd.none )

        AddProcess proc ->
            ( model, Api.addNode UpdatedGraph proc )


simpleButton : String -> msg -> Element msg
simpleButton s msg =
    Input.button [ Border.solid, Border.width 1, padding 10 ]
        { onPress = Just msg
        , label = text s
        }


displayNode : List Int -> Graph.Node UGen -> Element Msg
displayNode outputs n =
    row [ spacing 10 ]
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


volumeSlider : Float -> Element Msg
volumeSlider v =
    Input.slider
        [ width (px 200)
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
        { onChange = SetVolume
        , label = Input.labelAbove [] (text "Output amp")
        , min = 0.0
        , max = 1.0
        , value = v
        , thumb = Input.defaultThumb
        , step = Just 0.001
        }


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
        , addProcess "Gauss" Gauss
        , addProcess "RMS" RMS
        , addProcess "BitNeg" BitNeg
        , addProcess "BitOr" BitOr
        , addProcess "BitXOr" BitXOr
        , addProcess "BitAnd" BitAnd
        , delayInput m.delayLength
        , sinInput m.sinMul
        , linconInput m.linConA m.linConB
        , filterInput m.filterType m.filterFreq m.filterQ
        ]


view : Model -> Html Msg
view model =
    layout
        [ spacing 10
        , width fill
        , Font.size 14
        , Font.family
            [ Font.monospace
            ]
        ]
    <|
        column [ width fill, spacing 10 ]
            [ simpleButton "Randomize" Randomize
            , volumeSlider model.volume
            , processRow model
            , Maybe.withDefault none (Maybe.map (displayNode model.outputs) model.selectedNode)
            , case model.graph of
                Just g ->
                    html <|
                        DrawGraph.view g
                            (Maybe.map .id model.selectedNode)
                            SelectNode
                            SelectEdge
                            model.outputs
                            ( 1000, 800 )
                            ( 1600, 1100 )
                            200.0

                _ ->
                    none
            ]
