port module Midi exposing
    ( CC
    , Messages
    , cc
    , decodeCC
    , dict
    , send
    , sendEdgeState
    , sendEdges
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


port send : ( Int, Int, Int ) -> Cmd msg


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
    send ( 0, 4, outputAmp )
        :: List.indexedMap
            (\i p ->
                send
                    ( 0
                    , i + 5
                    , round
                        (Parameters.unmapped p p.value * 127)
                    )
            )
            (processParameters ugen.process)
        |> Cmd.batch


sendEdges : List (List Connection) -> Cmd msg
sendEdges connections =
    List.indexedMap
        (\ugenIdx ugenEdges ->
            List.indexedMap
                (\edgeIdx edge ->
                    --                    send ( 1, (ugenIdx * 8) + edgeIdx, round (edge.link.strength * 127) )
                    send
                        ( 1
                        , (ugenIdx * 8) + edgeIdx
                        , round (Parameters.uncubic (Parameters.linlin (Parameters.calcEdgeStrength edge.link.strength) -2.0 2.0 -1.0 1.0) * 127)
                        )
                )
                ugenEdges
                |> Cmd.batch
        )
        connections
        |> Cmd.batch


sendEdgeState : ProcessGraph.Link -> Cmd msg
sendEdgeState edge =
    let
        w =
            round (Parameters.uncubic (Parameters.linlin (Parameters.calcEdgeStrength edge.strength) -2.0 2.0 -1.0 1.0) * 127)

        _ =
            Debug.log "midi edge state: " w

        f =
            round (Parameters.explin edge.freq 1.0 20000.0 0.0 1.0 * 127)

        d =
            round (Parameters.explin edge.delay.delay 0.0 6.0 0.0 1.0 * 127)
    in
    List.map send [ ( 0, 25, f ), ( 0, 26, d ), ( 0, 27, w ) ]
        |> Cmd.batch


type alias Messages msg =
    { noMsg : msg
    , setNodeOutputAmp : Graph.NodeId -> Float -> msg
    , setProcessParameter : Graph.NodeId -> Int -> Float -> msg
    , setEdgeBias : Int -> Float -> msg
    , setEdgeFactor : Int -> Float -> msg
    , setEdgeDelay : Int -> Float -> msg
    , setEdgeFreq : Int -> Float -> msg
    , selectPrevNode : msg
    , selectNextNode : msg
    , selectPrevEdge : msg
    , selectNextEdge : msg
    , setVolume : Float -> msg
    , setInputGain : Float -> msg
    , setEdgeFac : Float -> msg
    , setOutputs : Int -> Float -> msg
    , setEdgeControlFactors : Int -> Float -> msg
    , setEdgeControlDelay : Int -> Float -> msg
    , setEdgeControlFreq : Int -> Float -> msg
    }


dict : Messages msg -> List (List Int) -> ( Maybe (Graph.Node UGen), Maybe (Graph.Edge Link) ) -> Dict ( Int, Int ) (Float -> msg)
dict messages edgeIndices ( node, edge ) =
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

        edgeB =
            \v ->
                Maybe.map
                    (\e ->
                        messages.setEdgeBias e.label.id
                            (Parameters.lincubic v -2.0 2.0)
                    )
                    edge
                    |> Maybe.withDefault messages.noMsg

        edgeFac =
            \v ->
                Maybe.map
                    (\e ->
                        messages.setEdgeFactor e.label.id
                            (Parameters.linlin v 0.0 1.0 0.0 6.0)
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
        [ ( ( 0, 29 )
          , \v ->
                if v > 0.5 then
                    messages.selectPrevNode

                else
                    messages.noMsg
          )
        , ( ( 0, 30 )
          , \v ->
                if v > 0.5 then
                    messages.selectNextNode

                else
                    messages.noMsg
          )
        , ( ( 0, 31 )
          , \v ->
                if v > 0.5 then
                    messages.selectPrevEdge

                else
                    messages.noMsg
          )
        , ( ( 0, 32 )
          , \v ->
                if v > 0.5 then
                    messages.selectNextEdge

                else
                    messages.noMsg
          )
        , ( ( 0, 0 )
          , \v ->
                messages.setVolume
                    (Parameters.linexp v 0.0 1.0 0.0 1.0)
          )
        , ( ( 0, 1 )
          , \v ->
                messages.setEdgeFac
                    (Parameters.linexp v 0.0 1.0 0.05 5.0)
          )
        , ( ( 0, 2 ), \v -> messages.setOutputs 0 v )
        , ( ( 0, 3 ), \v -> messages.setOutputs 1 v )
        , ( ( 0, 4 ), nodeAmp )
        , ( ( 0, 100 ), messages.setEdgeControlFactors 0 )
        , ( ( 0, 101 ), messages.setEdgeControlFactors 1 )
        , ( ( 0, 102 ), messages.setEdgeControlFactors 2 )
        , ( ( 0, 103 ), messages.setEdgeControlFactors 3 )
        , ( ( 0, 104 ), messages.setEdgeControlFactors 4 )
        , ( ( 0, 105 ), messages.setEdgeControlFactors 5 )
        , ( ( 0, 106 ), messages.setEdgeControlFactors 6 )
        , ( ( 0, 107 ), messages.setEdgeControlFactors 7 )
        , ( ( 0, 20 ), messages.setEdgeControlDelay 0 )
        , ( ( 0, 21 ), messages.setEdgeControlDelay 1 )
        , ( ( 0, 22 ), messages.setEdgeControlDelay 2 )
        , ( ( 0, 23 ), messages.setEdgeControlDelay 3 )

        -- , ( ( 0, 24 ), \v -> messages.setInputGain (Parameters.linexp v 0.0 1.0 0.0 6.0) )
        , ( ( 0, 24 ), edgeF )
        , ( ( 0, 25 ), edgeD )
        , ( ( 0, 26 ), edgeB )
        , ( ( 0, 27 ), edgeFac )

        -- , ( ( 0, 24 ), messages.setEdgeControlFreq 0 )
        -- , ( ( 0, 25 ), messages.setEdgeControlFreq 1 )
        -- , ( ( 0, 26 ), messages.setEdgeControlFreq 2 )
        -- , ( ( 0, 27 ), messages.setEdgeControlFreq 3 )
        ]
            ++ List.indexedMap (\i p -> ( ( 0, i + 5 ), p )) processPars



-- ++ (List.concat <|
--         List.indexedMap
--             (\ugenIdx ugenEdges ->
--                 List.indexedMap
--                     (\edgeIdx edgeId ->
--                         ( ( 1, (ugenIdx * 8) + edgeIdx )
--                         , messages.setEdgeWeight edgeId
--                         )
--                     )
--                     ugenEdges
--             )
--             edgeIndices
--    )
-- TODO clear dictonary of edgeids <> midicc
