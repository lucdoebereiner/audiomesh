module Matrix exposing (Matrix, UGenWithOutgoingConnections, groupEdges, holedIndices, matrixFromGraphs, ugenWithConnections, view)

--import Element.Background as Background

import Element exposing (..)
import Element.Border as Border
import Graph
import IntDict exposing (IntDict)
import List.Extra as L
import ProcessGraph
    exposing
        ( BackendGraph
        , Connection
        , Process
        , UGen
        , UGenGraph
        , graphEdges
        , ugenLabel
        , ugensWithIds
        )
import Utils exposing (..)



-- import Element.Font as Font
-- import Element.Input as Input


type alias Matrix =
    { ugens : IntDict UGen
    , connections : List Connection
    , nodeHoles : List Int
    }


holedIndices : Int -> List Int -> List Int
holedIndices n holes =
    List.range 0 n
        |> List.filter (\e -> not (List.member e holes))


type alias UGenWithOutgoingConnections =
    { ugen : UGen
    , id : Int
    , outgoing : List Connection
    }


ugenWithConnections : Matrix -> ( Int, UGen ) -> UGenWithOutgoingConnections
ugenWithConnections matrix ( id, ugen ) =
    { ugen = ugen
    , id = id
    , outgoing = List.filter (\c -> c.from == id) matrix.connections
    }


replaceUGensFromGraph : IntDict UGen -> UGenGraph -> IntDict UGen
replaceUGensFromGraph ugensDict graph =
    let
        nodesFromGraph =
            Graph.nodes graph

        nodesFromDict =
            IntDict.toList ugensDict
    in
    List.map
        (\( id, u ) ->
            case L.find (\grUGen -> grUGen.id == id) nodesFromGraph of
                Nothing ->
                    ( id, u )

                Just grUGen ->
                    ( id, grUGen.label )
        )
        nodesFromDict
        |> IntDict.fromList


matrixFromGraphs : BackendGraph -> UGenGraph -> Matrix
matrixFromGraphs gr ugenGr =
    let
        ugensFromGr =
            ugensWithIds gr.nodes gr.node_holes 0 IntDict.empty
    in
    { ugens = replaceUGensFromGraph ugensFromGr ugenGr
    , connections = graphEdges ugenGr
    , nodeHoles = gr.node_holes
    }


ugenRow : List Int -> UGenWithOutgoingConnections -> Element msg
ugenRow indices ugen =
    let
        bottomBorder =
            Border.widthEach { left = 0, right = 2, bottom = 2, top = 0 }

        connectionColumns =
            List.map
                (\idx ->
                    case L.find (\c -> c.to == idx) ugen.outgoing of
                        Nothing ->
                            column [ height (px 30), paddingXY 10 10, bottomBorder, centerX, centerY, width fill ] [ none ]

                        Just c ->
                            column [ height (px 30), paddingXY 10 10, bottomBorder, centerX, centerY, width fill ] [ paragraph [ centerX, width fill ] [ text (floatString c.link.strength) ] ]
                )
                indices
    in
    row [ Border.widthEach { left = 0, right = 0, bottom = 0, top = 0 }, width fill ]
        ([ column [ height (px 30), width (px 300), paddingXY 10 10, bottomBorder ] [ text (ugenLabel ugen.ugen) ]
         ]
            ++ connectionColumns
        )


horizontalLabels : List UGenWithOutgoingConnections -> Element msg
horizontalLabels ugens =
    let
        bottomBorder =
            Border.widthEach { left = 0, right = 2, bottom = 2, top = 0 }

        labels =
            List.map
                (\ugen ->
                    column [ height (px 50), paddingXY 10 10, bottomBorder, width fill ] [ paragraph [ width fill ] [ text (ugenLabel ugen.ugen) ] ]
                )
                ugens
    in
    row
        []
        ([ column [ height (px 50), bottomBorder, width (px 300) ] [ none ]
         ]
            ++ labels
        )


groupEdges : Matrix -> List (List Int)
groupEdges matrix =
    IntDict.toList
        matrix.ugens
        |> List.map (ugenWithConnections matrix)
        |> List.map .outgoing
        |> List.map (List.sortBy .to)
        |> List.map (List.map (.id << .link))


view : Matrix -> Element msg
view matrix =
    let
        ugensList =
            IntDict.toList
                matrix.ugens

        withConnections =
            List.map (ugenWithConnections matrix) ugensList
    in
    column [ centerX ]
        (horizontalLabels withConnections
            :: (withConnections
                    |> List.map (ugenRow (List.map Tuple.first ugensList))
               )
        )



-- ugenColumn : List Int -> UGenWithOutgoingConnections -> IndexedColumn UGenWithOutgoingConnections msg
-- ugenColumn indices ugen =
--     {
--         header = text (ugenLabel ugen)
--             , width = 20
--             , view = (\idx
--         }
-- type alias IndexedColumn record msg =
--  { header : Element msg
--  , width : Length
--  , view : Int -> record -> Element msg
--  }
