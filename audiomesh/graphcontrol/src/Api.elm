module Api exposing
    ( NodeIndex
    , addNode
    , connectLeastConnected
    , deleteNode
    , disconnectMostConnected
    , getGraph
    , getOutputs
    , randomize
    , setOutput
    , setParameter
    , setVolume
    )

import Http
import Json.Decode as Decode
import ProcessGraph exposing (..)


baseUrl : String
baseUrl =
    "http://localhost:8008"


type alias NodeIndex =
    Int


getGraph : (Result Http.Error BackendGraph -> msg) -> Cmd msg
getGraph msg =
    Http.get
        { url = baseUrl ++ "/graph"
        , expect = Http.expectJson msg decodeGraph
        }


deleteNode : (Result Http.Error () -> msg) -> NodeIndex -> Cmd msg
deleteNode msg n =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = baseUrl ++ "/node/" ++ String.fromInt n
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


connectLeastConnected : (Result Http.Error () -> msg) -> Cmd msg
connectLeastConnected msg =
    Http.post
        { url = baseUrl ++ "/connectleastconnected"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


disconnectMostConnected : (Result Http.Error () -> msg) -> Cmd msg
disconnectMostConnected msg =
    Http.post
        { url = baseUrl ++ "/disconnectmostconnected"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


setParameter : (Result Http.Error () -> msg) -> Int -> Int -> Float -> Cmd msg
setParameter msg ugenIdx parameter value =
    Http.post
        { url =
            baseUrl
                ++ "/node/"
                ++ String.fromInt ugenIdx
                ++ "/parameter/"
                ++ String.fromInt parameter
                ++ "/"
                ++ String.fromFloat value
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


setVolume : (Result Http.Error () -> msg) -> Float -> Cmd msg
setVolume msg amp =
    Http.post
        { url = baseUrl ++ "/volume/" ++ String.fromFloat amp
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


addNode : (Result Http.Error () -> msg) -> Process -> Cmd msg
addNode msg proc =
    Http.post
        { url = baseUrl ++ "/node"
        , body = Http.jsonBody (encodeProcess proc)
        , expect = Http.expectWhatever msg
        }


getOutputs : (Result Http.Error (List NodeIndex) -> msg) -> Cmd msg
getOutputs msg =
    Http.get
        { url = baseUrl ++ "/outputs"
        , expect = Http.expectJson msg (Decode.list Decode.int)
        }


setOutput : (Result Http.Error () -> msg) -> Int -> Int -> Cmd msg
setOutput msg id out =
    Http.post
        { url = baseUrl ++ "/node/" ++ String.fromInt id ++ "/output/" ++ String.fromInt out
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


randomize : (Result Http.Error () -> msg) -> Cmd msg
randomize msg =
    Http.post
        { url = baseUrl ++ "/randomize"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }
