module DrawGraph exposing (view)

import Color exposing (Color)
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeId)
import Html exposing (Html)
import IntDict
import List exposing (range)
import ProcessGraph exposing (Link, UGen, UGenGraph, defaultUGen, mkGraph, ugenLabel)
import Scale exposing (SequentialScale)
import Scale.Color
import TypedSvg exposing (a, circle, ellipse, g, line, polygon, polyline, rect, svg, text_, title)
import TypedSvg.Attributes exposing (class, cursor, fill, fontFamily, fontSize, fontWeight, height, noFill, points, stroke, textAnchor, viewBox, width, xlinkHref)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, rx, ry, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (AnchorAlignment(..), Cursor(..), FontWeight(..), Length(..), Paint(..), px)


type alias Entity =
    Force.Entity NodeId { value : UGen }


init : Graph UGen Link -> Float -> ( Float, Float ) -> Graph Entity Link
init contentGraph dist ( w, h ) =
    let
        graph =
            Graph.mapContexts
                (\({ node, incoming, outgoing } as ctx) ->
                    { incoming = incoming
                    , outgoing = outgoing
                    , node =
                        { label =
                            Force.entity node.id node.label
                        , id = node.id
                        }
                    }
                )
                contentGraph

        links =
            graph
                |> Graph.edges
                |> List.filter (\{ from, to, label } -> from /= to)
                |> List.map
                    (\{ from, to, label } ->
                        { source = from
                        , target = to
                        , distance = dist
                        , strength = Nothing --Just label.strength
                        }
                    )

        forces =
            [ Force.customLinks 1 links

            --              Force.links <| List.map link <| Graph.edges graph
            , Force.manyBodyStrength -2600 <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    Graph.nodes graph
        |> List.map .label
        |> Force.computeSimulation (Force.simulation forces)
        |> updateGraphWithList graph


updateGraphWithList : Graph Entity Link -> List Entity -> Graph Entity Link
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue : Graph.NodeContext Entity Link -> Entity -> Graph.NodeContext Entity Link
updateContextWithValue nodeCtx entValue =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = entValue } }


linkElement : Graph Entity Link -> (Int -> msg) -> Edge Link -> Svg msg
linkElement graph msg edge =
    let
        retrieveEntity =
            Maybe.withDefault (Force.entity 0 defaultUGen) << Maybe.map (.node >> .label)

        source =
            retrieveEntity <| Graph.get edge.from graph

        target =
            retrieveEntity <| Graph.get edge.to graph

        link =
            edge.label
    in
    if source == target then
        circle
            [ strokeWidth (3 * (edge.label.strength ^ 2))
            , stroke (Paint Color.black)
            , noFill
            , cx source.x
            , cy (source.y - 25)
            , r 40
            ]
            []

    else
        let
            ( centerX, centerY ) =
                ( source.x * 0.35 + target.x * 0.65, source.y * 0.35 + target.y * 0.65 )

            ( dx, dy ) =
                ( source.x - target.x, source.y - target.y )

            dist =
                250

            --sqrt (dx * dx + dy * dy)
            ( x3, y3 ) =
                ( centerX + (dy / dist * 20), centerY - (dx / dist * 20) )

            ( centerOffsetX, centerOffsetY ) =
                ( centerX - (dx / dist * 30), centerY - (dy / dist * 30) )
        in
        g []
            [ line
                [ strokeWidth (3 * (edge.label.strength ^ 2))
                , stroke (Paint Color.black) --<| Scale.convert colorScale source.x
                , x1 source.x
                , y1 source.y
                , x2 target.x
                , y2 target.y
                ]
                []
            , polyline
                [ strokeWidth 1
                , cursor CursorPointer
                , onClick (msg link.id)
                , stroke (Paint Color.black) --<| Scale.convert colorScale source.x
                , points
                    [ ( centerOffsetX, centerOffsetY )
                    , ( x3, y3 )
                    , ( centerX, centerY )
                    ]
                ]
                []
            ]


hexagon ( x, y ) size attrs =
    let
        angle =
            2 * pi / 6

        p =
            range 0 6
                |> List.map toFloat
                |> List.map (\a -> ( x + cos (a * angle) * size, y + sin (a * angle) * size ))
                |> points
    in
    polygon
        (p :: attrs)


nodeSize size node =
    hexagon ( node.x, node.y )
        size
        [ fill <| Paint Color.black --<| Scale.convert colorScale node.x
        ]
        [ title [] [ text node.value.name ] ]


nodeElement selectedId msg outputs node =
    let
        thisNode =
            ugenLabel node.label.value

        weight =
            if selectedId == Just node.id then
                FontWeightBolder

            else
                FontWeightNormal
    in
    g []
        [ ellipse
            ([ rx 130
             , ry 25
             , cx node.label.x
             , cy node.label.y
             , cursor CursorPointer
             , onClick (msg node.id)
             , stroke (Paint Color.black)
             ]
                ++ (if List.member node.id outputs then
                        [ strokeWidth 3.0, fill (Paint Color.grey) ]

                    else
                        [ strokeWidth 1.0, fill (Paint Color.white) ]
                   )
            )
            []
        , text_ [ textAnchor AnchorMiddle, x node.label.x, y (node.label.y + 5) ]
            [ a
                [ fontSize (Em 1)
                , fontFamily [ "Inconsolata" ]
                , fontWeight weight
                , onClick (msg node.id)
                , cursor CursorPointer
                ]
                [ text thisNode ]
            ]
        ]


viewGraph : ( Float, Float ) -> Maybe Int -> (Int -> msg) -> (Int -> msg) -> List Int -> ( Float, Float ) -> Graph Entity Link -> Html msg
viewGraph ( w, h ) selectedNode nodeSelectMsg linkSelectMsg outputs ( boxWidth, boxHeight ) model =
    svg [ width (Px w), height (Px h), viewBox 0 0 boxWidth boxHeight ]
        [ g [ class [ "links" ] ] <|
            List.map (linkElement model linkSelectMsg) <|
                Graph.edges model
        , g [ class [ "nodes" ] ] <|
            List.map (nodeElement selectedNode nodeSelectMsg outputs) <|
                Graph.nodes model
        ]


view :
    UGenGraph
    -> Maybe Int
    -> (Int -> msg)
    -> (Int -> msg)
    -> List Int
    -> ( Float, Float )
    -> ( Float, Float )
    -> Float
    -> Html msg
view graph selectedNode nodeSelectMsg linkSelectMsg outputs ( width, height ) ( boxWidth, boxHeight ) dist =
    viewGraph ( width, height )
        selectedNode
        nodeSelectMsg
        linkSelectMsg
        outputs
        ( boxWidth, boxHeight )
        (init graph dist ( boxWidth, boxHeight ))
