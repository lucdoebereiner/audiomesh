module Main exposing (Msg(..), main, update, view)

import Api
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import ProcessGraph exposing (..)


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


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
    | VoidReturn (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGraph res ->
            let
                _ =
                    Debug.log "graph" res
            in
            ( model, Cmd.none )

        GetGraph ->
            ( model, Api.getGraph GotGraph )

        Randomize ->
            ( model, Api.randomize VoidReturn )

        VoidReturn _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetGraph ] [ text "Get Graph" ]
        , button [ onClick Randomize ] [ text "Randomize" ]
        ]
