module OutputIndices exposing (OutputSpec, ampForIndex, encodeSpec, outputSpecs, updateGraphWithSpecs, updateUGenGraph)

import Array exposing (Array)
import Graph
import IntDict
import Json.Encode as JE exposing (Value)
import ProcessGraph exposing (UGenGraph, getNodes, isOutputProcess)


gaussCurve : Float -> Float
gaussCurve x =
    let
        c =
            0.23
    in
    e ^ ((x * x) / (-2.0 * (c * c)))


ampForIndex pos n index =
    if n <= 1 then
        1.0

    else
        let
            norm1 =
                toFloat index / toFloat n

            norm2 =
                toFloat (index + n) / toFloat n

            dist =
                min (abs (pos - norm1)) (abs (pos - norm2))
        in
        gaussCurve dist


type alias OutputSpec =
    { node : Graph.NodeId
    , output : Int
    , amp : Float
    }


updateUGenGraph : OutputSpec -> UGenGraph -> UGenGraph
updateUGenGraph spec g =
    Graph.update spec.node
        (Maybe.map
            (\ctx ->
                let
                    ugen =
                        ctx.node.label

                    prevnode =
                        ctx.node

                    newUgen =
                        ProcessGraph.upsertOutputSend ( spec.output, spec.amp ) ugen
                in
                { ctx | node = { prevnode | label = newUgen } }
            )
        )
        g


updateGraphWithSpecs : List OutputSpec -> UGenGraph -> UGenGraph
updateGraphWithSpecs specs g =
    List.foldl updateUGenGraph g specs


encodeSpec : OutputSpec -> Value
encodeSpec spec =
    JE.object
        [ ( "node", JE.int spec.node )
        , ( "output", JE.int spec.output )
        , ( "amp", JE.float spec.amp )
        ]


outputSpecsChannel : Float -> Int -> UGenGraph -> List OutputSpec
outputSpecsChannel pos output graph =
    let
        allNodes =
            List.map .id
                (List.filter
                    (\n ->
                        isOutputProcess n.label.process
                    )
                    (Graph.nodes graph)
                )
    in
    List.indexedMap
        (\i n ->
            { node = n
            , output = output
            , amp = ampForIndex pos (List.length allNodes) i
            }
        )
        allNodes


outputSpecs : Array Float -> UGenGraph -> List OutputSpec
outputSpecs positions graph =
    List.indexedMap (\i pos -> outputSpecsChannel pos i graph) (Array.toList positions)
        |> List.concat
