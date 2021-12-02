module EdgeControl exposing
    ( EdgeControl
    , addEdge
    , defaultControl
    , getCenter
    , getPositions
    , getSpread
    , setCenter
    , setSpread
    , updateFromGraph
    )

import Array exposing (Array)
import Array.Extra as A
import Graph
import ProcessGraph exposing (UGenGraph)
import Set exposing (Set)


type alias EdgeGroup =
    { edges : Set Int
    , spread : Float
    , center : Float
    }


type alias EdgeControl =
    Array EdgeGroup


defaultGroup =
    { edges = Set.empty, spread = 0.3, center = 0.5 }


defaultControl =
    Array.fromList [ defaultGroup, defaultGroup, defaultGroup, defaultGroup, defaultGroup, defaultGroup, defaultGroup, defaultGroup ]


setSpread : Int -> Float -> EdgeControl -> EdgeControl
setSpread i spread e =
    A.update i (\g -> { g | spread = spread }) e


setCenter : Int -> Float -> EdgeControl -> EdgeControl
setCenter i center e =
    A.update i (\g -> { g | center = center }) e


getSpread : Int -> EdgeControl -> Float
getSpread i e =
    Array.get i e
        |> Maybe.map .spread
        |> Maybe.withDefault 1.0


getCenter : Int -> EdgeControl -> Float
getCenter i e =
    Array.get i e
        |> Maybe.map .center
        |> Maybe.withDefault 1.0


addEdge : Int -> EdgeControl -> EdgeControl
addEdge i e =
    if
        Array.toList e
            |> List.any (\g -> Set.member i g.edges)
    then
        e

    else
        let
            idx =
                modBy 8 i

            length =
                Array.length e
        in
        if length >= idx then
            A.update idx (\g -> { g | edges = Set.insert i g.edges }) e

        else
            Array.push { edges = Set.fromList [ i ], spread = 0.3, center = 0.5 } e


getPositions : Int -> EdgeControl -> Maybe (List ( Int, Float ))
getPositions idx e =
    Array.get idx e
        |> Maybe.map groupPositions


groupPositions : EdgeGroup -> List ( Int, Float )
groupPositions g =
    let
        n =
            Set.size g.edges
    in
    List.indexedMap
        (\i e ->
            ( e, position i n g.spread g.center )
        )
        (Set.toList g.edges)


position : Int -> Int -> Float -> Float -> Float
position x n spread center =
    if n == 1 then
        center

    else
        let
            pos =
                (toFloat x * (spread / toFloat (n - 1)) + ((1.0 - spread) / 2.0))
                    + (center - 0.5)
        in
        min pos 1.0
            |> max 0.0


updateFromGraph : UGenGraph -> EdgeControl -> EdgeControl
updateFromGraph graph control =
    let
        edges =
            Graph.edges graph
                |> List.map (.label >> .id)
                |> List.sort
    in
    List.foldl addEdge control edges
