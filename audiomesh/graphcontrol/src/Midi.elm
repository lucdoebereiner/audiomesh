port module Midi exposing
    ( CC
    , Messages
    , cc
    , decodeCC
    , dict
    , send
    , sendEdgeState
    , sendProcessState
    )

import Dict exposing (Dict)
import Graph
import Json.Decode as Dec
import Json.Decode.Pipeline exposing (required)
import Parameters
import ProcessGraph exposing (..)


type alias CC =
    { channel : Int
    , controller : Int
    , value : Float
    }


port cc : (Dec.Value -> msg) -> Sub msg


port send : ( Int, Int ) -> Cmd msg


decodeCC : Dec.Value -> Result Dec.Error CC
decodeCC =
    Dec.decodeValue
        (Dec.succeed (\c co v -> { channel = c, controller = co, value = v })
            |> required "channel" Dec.int
            |> required "controller" Dec.int
            |> required "value" Dec.float
        )


sendProcessState : UGen -> Cmd msg
sendProcessState ugen =
    let
        outputAmp =
            Parameters.explin ugen.output_amp 0.0 2.0 0.0 1.0
                |> round
                << (*) 127
    in
    send ( 4, outputAmp )
        :: List.indexedMap
            (\i p ->
                send
                    ( i + 5
                    , round
                        (Parameters.unmapped p p.value * 127)
                    )
            )
            (processParameters ugen.process)
        |> Cmd.batch


sendEdgeState : ProcessGraph.Link -> Cmd msg
sendEdgeState edge =
    let
        w =
            round (Parameters.explin edge.strength 0.0 5.0 0.0 1.0 * 127)

        d =
            round (Parameters.explin edge.delay.delay 0.0 6.0 0.0 1.0 * 127)
    in
    List.map send [ ( 14, d ), ( 15, w ) ]
        |> Cmd.batch


type alias Messages msg =
    { noMsg : msg
    , setNodeOutputAmp : Graph.NodeId -> Float -> msg
    , setProcessParameter : Graph.NodeId -> Int -> Float -> msg
    , setEdgeWeight : Int -> Float -> msg
    , setEdgeDelay : Int -> Float -> msg
    , setEdgeFreq : Int -> Float -> msg
    , selectPrevNode : msg
    , selectNextNode : msg
    , selectPrevEdge : msg
    , selectNextEdge : msg
    , setVolume : Float -> msg
    , setEdgeFac : Float -> msg
    , setOutputs : Int -> Float -> msg
    , setEdgeControlWeights : Int -> Float -> msg
    , setEdgeControlDelay : Int -> Float -> msg
    , setEdgeControlFreq : Int -> Float -> msg
    }


dict : Messages msg -> ( Maybe (Graph.Node UGen), Maybe (Graph.Edge Link) ) -> Dict Int (Float -> msg)
dict messages ( node, edge ) =
    let
        nodeAmp =
            \val ->
                case node of
                    Nothing ->
                        messages.noMsg

                    Just n ->
                        messages.setNodeOutputAmp n.id (Parameters.linexp val 0.0 1.0 0.0 2.0)

        processPars =
            case node of
                Nothing ->
                    [ \_ -> messages.noMsg ]

                Just n ->
                    List.map
                        (\p ->
                            \v ->
                                messages.setProcessParameter n.id
                                    p.idx
                                    (Parameters.mapped { p | value = v })
                        )
                        (processParameters
                            n.label.process
                        )

        edgeW =
            \v ->
                Maybe.map
                    (\e ->
                        messages.setEdgeWeight e.label.id
                            (Parameters.linexp v 0.0 1.0 0.0 5.0)
                    )
                    edge
                    |> Maybe.withDefault messages.noMsg

        edgeD =
            \v ->
                Maybe.map
                    (\e ->
                        messages.setEdgeDelay e.label.id
                            (Parameters.linexp v 0.0 1.0 0.0 6.0)
                    )
                    edge
                    |> Maybe.withDefault messages.noMsg

        edgeF =
            \v ->
                Maybe.map
                    (\e ->
                        messages.setEdgeFreq e.label.id
                            (Parameters.linexp v 0.0 1.0 1.0 20000.0)
                    )
                    edge
                    |> Maybe.withDefault messages.noMsg
    in
    Dict.fromList <|
        [ ( 29
          , \v ->
                if v > 0.5 then
                    messages.selectPrevNode

                else
                    messages.noMsg
          )
        , ( 30
          , \v ->
                if v > 0.5 then
                    messages.selectNextNode

                else
                    messages.noMsg
          )
        , ( 31
          , \v ->
                if v > 0.5 then
                    messages.selectPrevEdge

                else
                    messages.noMsg
          )
        , ( 32
          , \v ->
                if v > 0.5 then
                    messages.selectNextEdge

                else
                    messages.noMsg
          )
        , ( 0
          , \v ->
                messages.setVolume
                    (Parameters.linexp v 0.0 1.0 0.0 1.0)
          )
        , ( 1
          , \v ->
                messages.setEdgeFac
                    (Parameters.linexp v 0.0 1.0 0.05 5.0)
          )
        , ( 2, \v -> messages.setOutputs 0 v )
        , ( 3, \v -> messages.setOutputs 1 v )
        , ( 4, nodeAmp )
        , ( 13, edgeF )
        , ( 14, edgeD )
        , ( 15, edgeW )
        , ( 16, messages.setEdgeControlWeights 0 )
        , ( 17, messages.setEdgeControlWeights 1 )
        , ( 18, messages.setEdgeControlWeights 2 )
        , ( 19, messages.setEdgeControlWeights 3 )
        , ( 20, messages.setEdgeControlDelay 0 )
        , ( 21, messages.setEdgeControlDelay 1 )
        , ( 22, messages.setEdgeControlDelay 2 )
        , ( 23, messages.setEdgeControlDelay 3 )
        , ( 24, messages.setEdgeControlFreq 0 )
        , ( 25, messages.setEdgeControlFreq 1 )
        , ( 26, messages.setEdgeControlFreq 2 )
        , ( 27, messages.setEdgeControlFreq 3 )
        ]
            ++ List.indexedMap (\i p -> ( i + 5, p )) processPars
