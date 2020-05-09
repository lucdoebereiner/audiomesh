module Api exposing (NodeIndex, deleteNode, getGraph, getOutputs, randomize, setOutput)

import Http
import Json.Decode as Decode
import ProcessGraph exposing (..)


baseUrl : String
baseUrl =
    "http://localhost:8001"



-- getGraph : (Result Http.Error Graph -> msg) -> Cmd msg
-- getGraph msg =
--     Http.request
--         { url = baseUrl ++ "/graph"
--         , method = "GET"
--         , headers = [ Http.header "Origin" "localhost" ]
--         , body = Http.emptyBody
--         , timeout = Nothing
--         , tracker = Nothing
--         , expect = Http.expectJson msg decodeGraph
--         }


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



-- randomize : (Result Http.Error () -> msg) -> Cmd msg
-- randomize msg =
--     Http.request
--         { url = baseUrl ++ "/randomize"
--         , method = "PUT"
--         , headers = []
--         , body = Http.emptyBody
--         , timeout = Nothing
--         , tracker = Nothing
--         , expect = Http.expectWhatever msg
--         }
