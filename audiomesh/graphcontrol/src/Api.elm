module Api exposing (getGraph, randomize)

import Http
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


getGraph : (Result Http.Error BackendGraph -> msg) -> Cmd msg
getGraph msg =
    Http.get
        { url = baseUrl ++ "/graph"
        , expect = Http.expectJson msg decodeGraph
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
