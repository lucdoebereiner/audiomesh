module Main exposing (Msg(..), main, update, view)

import Api
import Browser
import DrawGraph
import Graph
import Html exposing (Html, button, div, span, text)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing [] Nothing Nothing, Api.getGraph GotGraph )


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
            let
                _ =
                    Debug.log "graph" res
            in
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


displayNode : List Int -> Graph.Node UGen -> Html Msg
displayNode outputs n =
    div [ class "node-display " ]
        ([ div [ class "node-title" ] [ text (String.fromInt n.id ++ " " ++ ugenLabel n.label) ]
         ]
            ++ (if not (List.member n.id outputs) then
                    [ button [ class "delete-button", onClick (DeleteNode n.id) ]
                        [ text "Delete node" ]
                    ]
                        ++ (List.map
                                (\out ->
                                    button
                                        [ class "set-output", onClick (SetOutput n.id out) ]
                                        [ text ("As output " ++ String.fromInt out) ]
                                )
                            <|
                                List.range 0 (List.length outputs - 1)
                           )

                else
                    []
               )
        )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Randomize ] [ text "Randomize" ]
        , Maybe.withDefault (span [] []) (Maybe.map (displayNode model.outputs) model.selectedNode)
        , div [ class "graph-display" ]
            [ case model.graph of
                Just g ->
                    DrawGraph.view g
                        (Maybe.map .id model.selectedNode)
                        SelectNode
                        SelectEdge
                        model.outputs
                        ( 1000, 800 )
                        ( 1600, 1100 )
                        200.0

                _ ->
                    span [] []
            ]
        ]
