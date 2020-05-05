module Main exposing (Msg(..), main, update, view)

import Api
import Browser
import DrawGraph
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import ProcessGraph exposing (..)


type alias Model =
    Maybe BackendGraph


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, Api.getGraph GotGraph )


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
    | Randomize
    | Randomized (Result Http.Error ())


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
                    ( Just g, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GetGraph ->
            ( model, Api.getGraph GotGraph )

        Randomize ->
            ( model, Api.randomize Randomized )

        Randomized _ ->
            ( model, Api.getGraph GotGraph )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetGraph ] [ text "Get Graph" ]
        , button [ onClick Randomize ] [ text "Randomize" ]
        , div [ class "graph-display" ]
            [ case model of
                Just g ->
                    DrawGraph.view g "" ( 1000, 800 ) ( 1600, 1100 ) 200.0

                _ ->
                    span [] []
            ]
        ]
