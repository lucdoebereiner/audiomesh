module Api exposing
    ( NodeIndex
    , addNode
    , connectLeastConnected
    , deleteEdge
    , deleteNode
    , disconnectMostConnected
    , getGraph
    , getGraphForDownload
    , getOutputs
    , poll
    , postEdge
    , postGraph
    , randomize
    , setEdgeWeight
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


getGraphForDownload : (Result Http.Error String -> msg) -> Cmd msg
getGraphForDownload msg =
    Http.get
        { url = baseUrl ++ "/graph"
        , expect = Http.expectString msg
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


setEdgeWeight : (Result Http.Error () -> msg) -> Int -> Float -> Cmd msg
setEdgeWeight msg edgeIdx value =
    Http.post
        { url =
            baseUrl
                ++ "/edge/"
                ++ String.fromInt edgeIdx
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


postGraph : (Result Http.Error () -> msg) -> String -> Cmd msg
postGraph msg g =
    Http.post
        { url = baseUrl ++ "/graph"
        , body = Http.stringBody "application/json" g
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


postEdge : (Result Http.Error () -> msg) -> Int -> Int -> ( Int, Float ) -> Cmd msg
postEdge msg id1 id2 ( idx, w ) =
    Http.post
        { url =
            baseUrl
                ++ "/edge/"
                ++ String.fromInt id1
                ++ "/"
                ++ String.fromInt id2
                ++ "/"
                ++ String.fromFloat w
                ++ "/"
                ++ String.fromInt idx
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


deleteEdge : (Result Http.Error () -> msg) -> Int -> Cmd msg
deleteEdge msg e =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = baseUrl ++ "/edge/" ++ String.fromInt e
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


randomize : (Result Http.Error () -> msg) -> Cmd msg
randomize msg =
    Http.post
        { url = baseUrl ++ "/randomize"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


poll : (Result Http.Error Float -> msg) -> NodeIndex -> Cmd msg
poll msg n =
    Http.get
        { url = baseUrl ++ "/node/" ++ String.fromInt n ++ "/poll"
        , expect = Http.expectJson msg Decode.float
        }
