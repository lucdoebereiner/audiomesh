module ProcessGraph exposing (BackendGraph, ClipType(..), Connection, FilterType(..), Link, Process(..), UGen, decodeGraph, defaultUGen, mkGraph, ugenLabel)

import Array exposing (Array)
import Graph
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder, array, bool, field, float, index, int, list, oneOf, string)
import Json.Decode.Pipeline exposing (required)


type FilterType
    = BLPF
    | BHPF
    | BBPF


filterTypeFromString : String -> FilterType
filterTypeFromString s =
    case s of
        "BLPF" ->
            BLPF

        "BHPF" ->
            BHPF

        "BBPF" ->
            BBPF

        _ ->
            -- default
            BLPF


type ClipType
    = None
    | Wrap
    | SoftClip


clipTypeFromString : String -> ClipType
clipTypeFromString s =
    case s of
        "Wrap" ->
            Wrap

        "SoftClip" ->
            SoftClip

        _ ->
            None


type Process
    = Mem { input : Float, last_value : Float }
    | Delay { rec_idx : Int }
    | Add { inputs : List Float }
    | Filter { filter_type : FilterType, freq : Float, q : Float }
    | Gauss { input : Float }


processToString : Process -> String
processToString p =
    case p of
        Mem _ ->
            "Mem"

        Delay _ ->
            "Delay"

        Add _ ->
            "Add"

        Filter _ ->
            "Filter"

        Gauss _ ->
            "Gauss"


decodeMem : Decoder Process
decodeMem =
    Decode.succeed (\input last_value -> Mem { input = input, last_value = last_value })
        |> required "input" float
        |> required "last_value" float


decodeDelay : Decoder Process
decodeDelay =
    Decode.succeed (\rec_idx -> Delay { rec_idx = rec_idx })
        |> required "rec_idx" int


decodeAdd : Decoder Process
decodeAdd =
    Decode.succeed (\inputs -> Add { inputs = inputs })
        |> required "inputs" (list float)


decodeGauss : Decoder Process
decodeGauss =
    Decode.succeed (\input -> Gauss { input = input })
        |> required "input" float


decodeFilter : Decoder Process
decodeFilter =
    Decode.succeed
        (\ftype freq q ->
            Filter
                { filter_type = filterTypeFromString ftype
                , freq = freq
                , q = q
                }
        )
        |> required "filter_type" string
        |> required "freq" float
        |> required "q" float


type alias UGen =
    { process : Process
    , sum_inputs : Bool
    , clip : ClipType
    }


ugenLabel : UGen -> String
ugenLabel u =
    processToString u.process


defaultUGen : UGen
defaultUGen =
    UGen (Add { inputs = [] }) True None


decodeUGen : Decoder UGen
decodeUGen =
    Decode.succeed (\p sum clip -> UGen p sum (clipTypeFromString clip))
        |> required "process"
            (oneOf
                [ field "Delay" decodeDelay
                , field "Add" decodeAdd
                , field "Mem" decodeMem
                , field "Gauss" decodeGauss
                , field "Filter" decodeGauss
                ]
            )
        |> required "sum_inputs" bool
        |> required "clip" string


type alias Link =
    { index : Int
    , strength : Float
    }


linkFromArray : Array Float -> Link
linkFromArray ar =
    let
        idx =
            Array.get 0 ar

        str =
            Array.get 1 ar
    in
    case ( idx, str ) of
        ( Just l_index, Just strength ) ->
            Link (floor l_index) strength

        _ ->
            Link 0 1.0


type alias Connection =
    { from : Int
    , to : Int
    , link : Link
    }


decodeConnection : Decoder Connection
decodeConnection =
    Decode.map3
        (\from to link ->
            Connection from to (linkFromArray link)
        )
        (index 0 int)
        (index 1 int)
        (index 2 (array float))


type alias BackendGraph =
    { nodes : List UGen
    , node_holes : List Int
    , edges : List Connection
    }


decodeGraph : Decoder BackendGraph
decodeGraph =
    Decode.succeed BackendGraph
        |> required "nodes" (list decodeUGen)
        |> required "node_holes" (list int)
        |> required "edges" (list decodeConnection)


ugensWithIds : List UGen -> List Int -> Int -> IntDict UGen -> IntDict UGen
ugensWithIds ugens node_holes curr_idx dict =
    case ugens of
        [] ->
            dict

        u :: rest_ugens ->
            if List.member curr_idx node_holes then
                ugensWithIds ugens node_holes (curr_idx + 1) dict

            else
                ugensWithIds rest_ugens
                    node_holes
                    (curr_idx + 1)
                    (IntDict.insert curr_idx u dict)


getNodes : BackendGraph -> List (Graph.Node UGen)
getNodes gr =
    ugensWithIds gr.nodes gr.node_holes 0 IntDict.empty
        |> IntDict.toList
        |> List.map (\( k, v ) -> Graph.Node k v)


mkGraph : BackendGraph -> Graph.Graph UGen Link
mkGraph gr =
    let
        nodes =
            getNodes gr
    in
    Graph.fromNodesAndEdges nodes <|
        List.map (\e -> Graph.Edge e.from e.to e.link) gr.edges
