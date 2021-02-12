module ProcessGraph exposing (BackendGraph, ClipType(..), Connection, FilterType(..), Link, Process(..), UGen, UGenGraph, decodeGraph, defaultUGen, mkGraph, ugenLabel)

import Array exposing (Array)
import Graph
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder, array, bool, field, float, index, int, list, nullable, oneOf, string)
import Json.Decode.Pipeline exposing (hardcoded, required)


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


filterTypeToString : FilterType -> String
filterTypeToString ft =
    case ft of
        BLPF ->
            "BLPF"

        BHPF ->
            "BHPF"

        BBPF ->
            "BBPF"


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
    | Delay { length : Int, rec_idx : Int }
    | Add { inputs : List Float }
    | Mul { inputs : List Float }
    | Filter { filter_type : FilterType, freq : Float, q : Float }
    | Gauss { input : Float }
    | RMS
    | Sin
    | Constant { input : Float }
    | SoundIn { index : Int }


processToString : Process -> String
processToString p =
    case p of
        Constant c ->
            "Const " ++ String.fromFloat c.input

        RMS ->
            "RMS"

        Sin ->
            "Sin"

        Mem _ ->
            "Mem"

        Delay d ->
            "Delay length:" ++ String.fromInt d.length

        Add _ ->
            "Add"

        Mul _ ->
            "Mul"

        Filter f ->
            filterTypeToString f.filter_type
                ++ " freq:"
                ++ String.fromFloat f.freq
                ++ " q:"
                ++ String.fromFloat f.q

        Gauss _ ->
            "Gauss"

        SoundIn i ->
            "SoundIn " ++ String.fromInt i.index


decodeMem : Decoder Process
decodeMem =
    Decode.succeed (\input last_value -> Mem { input = input, last_value = last_value })
        |> required "input" float
        |> required "last_value" float


decodeDelay : Decoder Process
decodeDelay =
    Decode.succeed (\l rec_idx -> Delay { length = l, rec_idx = rec_idx })
        |> required "input" int
        |> required "rec_idx" int


decodeAdd : Decoder Process
decodeAdd =
    Decode.succeed (\inputs -> Add { inputs = inputs })
        |> required "inputs" (list float)


decodeSoundIn : Decoder Process
decodeSoundIn =
    Decode.succeed (\input -> SoundIn { index = input })
        |> required "index" int


decodeMul : Decoder Process
decodeMul =
    Decode.succeed (\inputs -> Mul { inputs = inputs })
        |> required "inputs" (list float)


decodeConstant : Decoder Process
decodeConstant =
    Decode.succeed (\input -> Constant { input = input })
        |> required "input" float


decodeRMS : Decoder Process
decodeRMS =
    Decode.succeed RMS


decodeSin : Decoder Process
decodeSin =
    Decode.succeed Sin


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
                , field "Mul" decodeMul
                , field "RMS" decodeRMS
                , field "Constant" decodeConstant
                , field "Sin" decodeSin
                , field "Gauss" decodeGauss
                , field "SoundIn" decodeSoundIn
                , field "Filter" (field "filter" decodeFilter)
                ]
            )
        |> required "sum_inputs" bool
        |> required "clip" string


type alias Link =
    { index : Int
    , strength : Float
    , id : Int
    }


type alias Connection =
    { from : Int
    , to : Int
    , link : Link
    }


type IntOrFloatArray
    = IntElement Int
    | FloatArray (Array Float)


decodeIntElement : Decoder IntOrFloatArray
decodeIntElement =
    Decode.map IntElement int


decodeFloatArray : Decoder IntOrFloatArray
decodeFloatArray =
    Decode.map FloatArray (array float)


decodeLink : Decoder (Array IntOrFloatArray)
decodeLink =
    array (oneOf [ decodeFloatArray, decodeIntElement ])


decodeConnection : Decoder (Maybe (Array IntOrFloatArray))
decodeConnection =
    nullable decodeLink


connectionFromArray : Array IntOrFloatArray -> Int -> Maybe Connection
connectionFromArray ar id =
    let
        getInt =
            Maybe.andThen
                (\a ->
                    case a of
                        IntElement i ->
                            Just i

                        _ ->
                            Nothing
                )

        fromM =
            getInt <| Array.get 0 ar

        toM =
            getInt <| Array.get 1 ar

        idxStrength =
            Maybe.map (\a -> ( Array.get 0 a, Array.get 1 a )) <|
                Maybe.andThen
                    (\a ->
                        case a of
                            FloatArray f ->
                                Just f

                            _ ->
                                Nothing
                    )
                    (Array.get 2 ar)
    in
    case ( fromM, toM, idxStrength ) of
        ( Just from, Just to, Just ( Just idx, Just strength ) ) ->
            Just <| Connection from to (Link (floor idx) strength id)

        _ ->
            Nothing



-- (\from to link ->
--     Connection from to (linkFromArray link)
-- )
-- (index 0 int)
-- (index 1 int)
-- (index 2 (array float))


type alias BackendGraph =
    { nodes : List UGen
    , node_holes : List Int
    , edges : List Connection
    , outputs : List Int
    }


connectionsFromArrays : List (Maybe (Array IntOrFloatArray)) -> Int -> List Connection
connectionsFromArrays arrays idx =
    case arrays of
        [] ->
            []

        Nothing :: rest ->
            connectionsFromArrays rest (idx + 1)

        (Just ar) :: rest ->
            case connectionFromArray ar idx of
                Just c ->
                    c :: connectionsFromArrays rest (idx + 1)

                Nothing ->
                    connectionsFromArrays rest (idx + 1)


decodeGraph : Decoder BackendGraph
decodeGraph =
    Decode.succeed (\n nh e outs -> BackendGraph n nh (connectionsFromArrays e 0) outs)
        |> required "nodes" (list decodeUGen)
        |> required "node_holes" (list int)
        |> required "edges" (list decodeConnection)
        |> hardcoded []


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


type alias UGenGraph =
    Graph.Graph UGen Link


mkGraph : BackendGraph -> UGenGraph
mkGraph gr =
    let
        nodes =
            getNodes gr
    in
    Graph.fromNodesAndEdges nodes <|
        List.map (\e -> Graph.Edge e.from e.to e.link) gr.edges
