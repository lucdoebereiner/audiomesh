module Api exposing
    ( NodeIndex
    , addNode
    , connectLeastConnected
    , deleteEdge
    , deleteNode
    , disconnectMostConnected
    , getGraph
    , getGraphForDownload
    , getMatrixMode
    , poll
    , postEdge
    , postGraph
    , randomCircle
    , randomize
    , setEdgeDelay
    , setEdgeFac
    , setEdgeFreq
    , setEdgeWeight
    , setMatrixMode
    , setNodeOutputAmp
    , setOutputs
    , setParameter
    , setVolume
    )

import Http
import Json.Decode as Decode
import Json.Encode as JE
import OutputIndices
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


getMatrixMode : (Result Http.Error Bool -> msg) -> Cmd msg
getMatrixMode msg =
    Http.get
        { url = baseUrl ++ "/matrix"
        , expect = Http.expectJson msg Decode.bool
        }


setMatrixMode : (Result Http.Error () -> msg) -> Bool -> Cmd msg
setMatrixMode msg mode =
    Http.post
        { url =
            baseUrl
                ++ "/matrix/"
                ++ (if mode then
                        "on"

                    else
                        "off"
                   )
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


setNodeOutputAmp : (Result Http.Error () -> msg) -> Int -> Float -> Cmd msg
setNodeOutputAmp msg ugenIdx amp =
    Http.post
        { url =
            baseUrl
                ++ "/node/"
                ++ String.fromInt ugenIdx
                ++ "/outputamp/"
                ++ String.fromFloat amp
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
                ++ "/weight/"
                ++ String.fromFloat value
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


setEdgeFreq : (Result Http.Error () -> msg) -> Int -> Float -> Cmd msg
setEdgeFreq msg edgeIdx value =
    Http.post
        { url =
            baseUrl
                ++ "/edge/"
                ++ String.fromInt edgeIdx
                ++ "/freq/"
                ++ String.fromFloat value
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


setEdgeDelay : (Result Http.Error () -> msg) -> Int -> Float -> Cmd msg
setEdgeDelay msg edgeIdx value =
    Http.post
        { url =
            baseUrl
                ++ "/edge/"
                ++ String.fromInt edgeIdx
                ++ "/delay/"
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


setEdgeFac : (Result Http.Error () -> msg) -> Float -> Cmd msg
setEdgeFac msg amp =
    Http.post
        { url = baseUrl ++ "/edgefac/" ++ String.fromFloat amp
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



-- setOutput : (Result Http.Error () -> msg) -> Int -> Int -> Cmd msg
-- setOutput msg id out =
--     Http.post
--         { url = baseUrl ++ "/node/" ++ String.fromInt id ++ "/output/" ++ String.fromInt out
--         , body = Http.emptyBody
--         , expect = Http.expectWhatever msg
--         }


setOutputs : (Result Http.Error () -> msg) -> List OutputIndices.OutputSpec -> Cmd msg
setOutputs msg specs =
    Http.post
        { url = baseUrl ++ "/outputs"
        , body = Http.jsonBody (JE.list OutputIndices.encodeSpec <| specs)
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


randomCircle : (Result Http.Error () -> msg) -> Cmd msg
randomCircle msg =
    Http.post
        { url = baseUrl ++ "/randomcircle"
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        }


poll : (Result Http.Error Float -> msg) -> NodeIndex -> Cmd msg
poll msg n =
    Http.get
        { url = baseUrl ++ "/node/" ++ String.fromInt n ++ "/poll"
        , expect = Http.expectJson msg Decode.float
        }
