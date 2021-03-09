module ProcessGraph exposing
    ( BackendGraph
    , ClipType(..)
    , Connection
    , FilterType(..)
    , Link
    , Process(..)
    , UGen
    , UGenGraph
    , decodeGraph
    , defaultUGen
    , encodeProcess
    , getNodes
    , mkGraph
    , mulAllEdges
    , nextEdge
    , nextNode
    , prevEdge
    , prevNode
    , processParameters
    , setInput
    , ugenLabel
    , updateEdge
    , updateProcessParameter
    , upsertOutputSend
    )

import Array exposing (Array)
import Graph
import IntDict exposing (IntDict)
import Json.Decode as Decode
    exposing
        ( Decoder
        , array
        , bool
        , field
        , float
        , index
        , int
        , list
        , nullable
        , oneOf
        , string
        )
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as JE exposing (Value)
import List.Extra as L
import Parameters exposing (Mapping(..), Parameter)
import Utils exposing (..)


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
    = Mem { lastValue : Float }
    | Delay { length : Int }
    | Add
    | Mul
    | Softclip
    | Ducking
    | EnvFollow
    | PLL { factor : Float }
    | Ring
    | Filter { filterType : FilterType, freq : Float, q : Float }
    | Gauss
    | RMS
    | Sin { mul : Float }
    | SinOsc { freq : Float, freq_mul : Float }
    | Constant { value : Float }
    | SoundIn { index : Int, factor : Float }
    | Square
    | BitNeg
    | BitOr
    | BitXOr
    | BitAnd
    | LinCon { linconA : Float, linconB : Float }
    | Compressor { threshold : Float, ratio : Float, makeup : Float }
    | Spike { threshold : Float, tConst : Float, r : Float, tRest : Int }



-- type alias Parameter =
--     { idx : Int
--     , val : Float
--     , name : String
--     , min : Float
--     , max : Float
--     }


processParameters : Process -> List Parameter
processParameters p =
    case p of
        Mem { lastValue } ->
            [ Parameter 1 lastValue Lin "Last" -1.0 1.0 ]

        Sin { mul } ->
            [ Parameter 1 mul Exp "Mul" 0.001 10.0 ]

        PLL { factor } ->
            [ Parameter 1 factor Lin "Fac" 0.01 1.0 ]

        SoundIn { index, factor } ->
            [ Parameter 1 factor Exp "Fac" 0.01 10.0 ]

        SinOsc { freq, freq_mul } ->
            [ Parameter 1 freq Exp "Freq" 0.001 10000.0
            , Parameter 2 freq_mul Exp "Freq Mul" 0.1 10000.0
            ]

        -- Constant { value } ->
        --     [ Parameter 1 value Lin "Value" -1.0 1.0 ]
        Filter { filterType, freq, q } ->
            [ Parameter 1 freq Exp "Freq" 5.0 4000.0, Parameter 2 q Exp "Q" 0.1 20.0 ]

        LinCon { linconA, linconB } ->
            [ Parameter 1 linconA Exp "A" 0.001 10.0, Parameter 2 linconB Exp "B" 0.001 20.0 ]

        Compressor { threshold, ratio, makeup } ->
            [ Parameter 1 threshold Exp "Threshold" 0.01 0.9
            , Parameter 2 ratio Exp "Ratio" 1.0 20.0
            , Parameter 3 makeup Exp "Makup" 1.0 4.0
            ]

        Spike { threshold, tConst, r, tRest } ->
            [ Parameter 1 threshold Exp "Threshold" 0.01 0.9
            , Parameter 2 tConst Exp "T Const" 0.00001 0.5
            , Parameter 3 r Exp "R" 0.1 20
            , Parameter 4 (toFloat tRest) Exp "Rest" 10 40000
            ]

        _ ->
            []


setInput : ( Int, Float ) -> Process -> Process
setInput ( parIdx, val ) proc =
    case proc of
        Mem _ ->
            Mem { lastValue = val }

        Sin _ ->
            Sin { mul = val }

        PLL _ ->
            PLL { factor = val }

        SoundIn s ->
            SoundIn { s | factor = val }

        SinOsc s ->
            case parIdx of
                1 ->
                    SinOsc { s | freq = val }

                2 ->
                    SinOsc { s | freq_mul = val }

                _ ->
                    proc

        Constant _ ->
            Constant { value = val }

        Filter f ->
            case parIdx of
                1 ->
                    Filter { f | freq = val }

                2 ->
                    Filter { f | q = val }

                _ ->
                    proc

        LinCon l ->
            case parIdx of
                1 ->
                    LinCon { l | linconA = val }

                2 ->
                    LinCon { l | linconB = val }

                _ ->
                    proc

        Compressor c ->
            case parIdx of
                1 ->
                    Compressor { c | threshold = val }

                2 ->
                    Compressor { c | ratio = val }

                3 ->
                    Compressor { c | makeup = val }

                _ ->
                    proc

        Spike s ->
            case parIdx of
                1 ->
                    Spike { s | threshold = val }

                2 ->
                    Spike { s | tConst = val }

                3 ->
                    Spike { s | r = val }

                4 ->
                    Spike { s | tRest = round val }

                _ ->
                    proc

        _ ->
            proc


processName : Process -> String
processName p =
    case p of
        Mem _ ->
            "Mem"

        Delay _ ->
            "Delay"

        Add ->
            "Add"

        PLL _ ->
            "PLL"

        Mul ->
            "Mul"

        Ducking ->
            "Ducking"

        EnvFollow ->
            "EnvFollow"

        Softclip ->
            "Softclip"

        Ring ->
            "Ring"

        Filter _ ->
            "Filter"

        Gauss ->
            "Gauss"

        RMS ->
            "RMS"

        Sin _ ->
            "Sin"

        SinOsc _ ->
            "SinOsc"

        Constant _ ->
            "Constant"

        SoundIn _ ->
            "SoundIn"

        Square ->
            "Square"

        BitNeg ->
            "BitNeg"

        BitOr ->
            "BitOr"

        BitXOr ->
            "BitXOr"

        BitAnd ->
            "BitAnd"

        LinCon _ ->
            "LinCon"

        Compressor _ ->
            "Compressor"

        Spike _ ->
            "Spike"


encodeProcess : Process -> Value
encodeProcess p =
    let
        encObj proc ob =
            JE.object [ ( processName proc, ob ) ]
    in
    case p of
        Mem m ->
            encObj p (JE.object [ ( "last_value", JE.float m.lastValue ) ])

        Delay i ->
            encObj p (JE.object [ ( "input", JE.int i.length ) ])

        Filter f ->
            encObj p
                (JE.object
                    [ ( "filter_type", JE.string (filterTypeToString f.filterType) )
                    , ( "freq", JE.float f.freq )
                    , ( "q", JE.float f.q )
                    ]
                )

        Sin m ->
            encObj p (JE.object [ ( "mul", JE.float m.mul ) ])

        PLL pll ->
            encObj p (JE.object [ ( "factor", JE.float pll.factor ) ])

        SinOsc s ->
            encObj p
                (JE.object
                    [ ( "freq", JE.float s.freq )
                    , ( "freq_mul", JE.float s.freq_mul )
                    ]
                )

        Constant c ->
            encObj p (JE.object [ ( "value", JE.float c.value ) ])

        SoundIn s ->
            encObj p (JE.object [ ( "index", JE.int s.index ), ( "factor", JE.float s.factor ) ])

        LinCon l ->
            encObj p
                (JE.object
                    [ ( "lincon_a", JE.float l.linconA )
                    , ( "lincon_b", JE.float l.linconB )
                    ]
                )

        Compressor c ->
            encObj p
                (JE.object
                    [ ( "threshold", JE.float c.threshold )
                    , ( "ratio", JE.float c.ratio )
                    , ( "makeup", JE.float c.makeup )
                    ]
                )

        Spike s ->
            encObj p
                (JE.object
                    [ ( "threshold", JE.float s.threshold )
                    , ( "t_const", JE.float s.tConst )
                    , ( "r", JE.float s.r )
                    , ( "t_rest", JE.int s.tRest )
                    ]
                )

        _ ->
            encObj p (JE.object [])


processToString : Process -> String
processToString p =
    case p of
        Constant c ->
            "Const " ++ floatString c.value

        Delay d ->
            "Delay length:" ++ String.fromInt d.length

        Filter f ->
            filterTypeToString f.filterType
                ++ " freq:"
                ++ floatString f.freq
                ++ " q:"
                ++ floatString f.q

        SoundIn i ->
            "SoundIn "
                ++ String.fromInt i.index
                ++ " fac: "
                ++ floatString i.factor

        SinOsc f ->
            "SinOsc fr:"
                ++ floatString f.freq
                ++ " fr_mul:"
                ++ floatString f.freq_mul

        Sin s ->
            "Sin " ++ floatString s.mul

        PLL pll ->
            "PLL fac: " ++ floatString pll.factor

        LinCon l ->
            "LinCon a: "
                ++ floatString l.linconA
                ++ " b: "
                ++ floatString l.linconB

        Compressor c ->
            "Compr thr: "
                ++ floatString c.threshold
                ++ " r: "
                ++ floatString c.ratio
                ++ " m: "
                ++ floatString c.makeup

        Spike s ->
            "Spike thr: "
                ++ floatString s.threshold
                ++ " tc: "
                ++ floatStringLong s.tConst
                ++ " r:"
                ++ floatString s.r
                ++ " tr: "
                ++ String.fromInt s.tRest

        _ ->
            processName p


decodeMem : Decoder Process
decodeMem =
    Decode.succeed (\last_value -> Mem { lastValue = last_value })
        |> required "last_value" float


decodeDelay : Decoder Process
decodeDelay =
    Decode.succeed (\l -> Delay { length = l })
        |> required "input" int


decodeSoftclip : Decoder Process
decodeSoftclip =
    Decode.succeed Softclip


decodeSoundIn : Decoder Process
decodeSoundIn =
    Decode.succeed (\input f -> SoundIn { index = input, factor = f })
        |> required "index" int
        |> required "factor" float


decodeSimpleProcess : Process -> Decoder Process
decodeSimpleProcess p =
    Decode.succeed p


decodeSquare : Decoder Process
decodeSquare =
    Decode.succeed Square


decodeBitAnd : Decoder Process
decodeBitAnd =
    Decode.succeed BitAnd


decodeBitNeg : Decoder Process
decodeBitNeg =
    Decode.succeed BitNeg


decodeBitOr : Decoder Process
decodeBitOr =
    Decode.succeed BitOr


decodeBitXOr : Decoder Process
decodeBitXOr =
    Decode.succeed BitXOr


decodeConstant : Decoder Process
decodeConstant =
    Decode.succeed (\input -> Constant { value = input })
        |> required "value" float


decodeSin : Decoder Process
decodeSin =
    Decode.succeed (\m -> Sin { mul = m })
        |> required "mul" float


decodePLL : Decoder Process
decodePLL =
    Decode.succeed (\f -> PLL { factor = f })
        |> required "factor" float


decodeFilter : Decoder Process
decodeFilter =
    Decode.succeed
        (\ftype freq q ->
            Filter
                { filterType = filterTypeFromString ftype
                , freq = freq
                , q = q
                }
        )
        |> required "filter_type" string
        |> required "freq" float
        |> required "q" float


decodeSinOsc : Decoder Process
decodeSinOsc =
    Decode.succeed (\f m -> SinOsc { freq = f, freq_mul = m })
        |> required "freq" float
        |> required "freq_mul" float


decodeLinCon : Decoder Process
decodeLinCon =
    Decode.succeed (\a b -> LinCon { linconA = a, linconB = b })
        |> required "lincon_a" float
        |> required "lincon_b" float


decodeCompressor : Decoder Process
decodeCompressor =
    Decode.succeed (\t r m -> Compressor { threshold = t, ratio = r, makeup = m })
        |> required "threshold" float
        |> required "ratio" float
        |> required "makeup" float


decodeSpike : Decoder Process
decodeSpike =
    Decode.succeed (\t c r rest -> Spike { threshold = t, tConst = c, r = r, tRest = rest })
        |> required "threshold" float
        |> required "t_const" float
        |> required "r" float
        |> required "t_rest" int


type alias UGen =
    { process : Process
    , sum_inputs : Bool
    , clip : ClipType
    , output_sends : IntDict Float
    }


upsertOutputSend : ( Int, Float ) -> UGen -> UGen
upsertOutputSend ( k, v ) ugen =
    if IntDict.member k ugen.output_sends then
        { ugen | output_sends = IntDict.update k (always (Just v)) ugen.output_sends }

    else
        { ugen | output_sends = IntDict.insert k v ugen.output_sends }


ugenLabel : UGen -> String
ugenLabel u =
    processToString u.process


defaultUGen : UGen
defaultUGen =
    UGen Add True None IntDict.empty


decodeOutputTuple =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.float)


decodeUGen : Decoder UGen
decodeUGen =
    Decode.succeed (\p sum clip sends -> UGen p sum (clipTypeFromString clip) (IntDict.fromList sends))
        |> required "process"
            (oneOf
                [ field "Delay" decodeDelay
                , field "Add" (decodeSimpleProcess Add)
                , field "Mem" decodeMem
                , field "Mul" (decodeSimpleProcess Mul)
                , field "Softclip" decodeSoftclip
                , field "Ring" (decodeSimpleProcess Ring)
                , field "RMS" (decodeSimpleProcess RMS)
                , field "Constant" decodeConstant
                , field "Compressor" decodeCompressor
                , field "Spike" decodeSpike
                , field "Sin" decodeSin
                , field "Ducking" (decodeSimpleProcess Ducking)
                , field "Gauss" (decodeSimpleProcess Gauss)
                , field "EnvFollow" (decodeSimpleProcess EnvFollow)
                , field "PLL" decodePLL
                , field "SoundIn" decodeSoundIn
                , field "Filter" decodeFilter
                , field "SinOsc" decodeSinOsc
                , field "LinCon" decodeLinCon
                , field "Square" decodeSquare
                , field "BitAnd" decodeBitAnd
                , field "BitOr" decodeBitOr
                , field "BitXOr" decodeBitXOr
                , field "BitNeg" decodeBitNeg
                ]
            )
        |> required "sum_inputs" bool
        |> required "clip" string
        |> required "output_sends" (list decodeOutputTuple)


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


nextNode : UGenGraph -> Maybe Graph.NodeId -> Maybe Graph.NodeId
nextNode g nId =
    let
        nodes =
            Graph.nodeIds g |> List.sort
    in
    case nId of
        Nothing ->
            List.head nodes

        Just n ->
            if nId == L.last nodes then
                List.head nodes

            else
                L.find (\other -> other > n) nodes


nextEdge : UGenGraph -> Maybe Int -> Maybe Int
nextEdge g eId =
    let
        edges =
            Graph.edges g |> List.map (.label >> .id) |> List.sort
    in
    case eId of
        Nothing ->
            List.head edges

        Just e ->
            if eId == L.last edges then
                List.head edges

            else
                L.find (\other -> other > e) edges


prevEdge : UGenGraph -> Maybe Int -> Maybe Int
prevEdge g eId =
    let
        edges =
            Graph.edges g |> List.map (.label >> .id) |> List.sort |> List.reverse
    in
    case eId of
        Nothing ->
            List.head edges

        Just e ->
            if eId == L.last edges then
                List.head edges

            else
                L.find (\other -> other < e) edges


prevNode : UGenGraph -> Maybe Graph.NodeId -> Maybe Graph.NodeId
prevNode g nId =
    let
        nodes =
            Graph.nodeIds g |> List.sort |> List.reverse
    in
    case nId of
        Nothing ->
            List.head nodes

        Just n ->
            if nId == L.last nodes then
                List.head nodes

            else
                L.find (\other -> other < n) nodes


updateProcessParameter : Graph.NodeId -> ( Int, Float ) -> UGenGraph -> UGenGraph
updateProcessParameter id ( parIdx, val ) graph =
    Graph.update id
        (Maybe.map
            (\ugenCtx ->
                let
                    node =
                        ugenCtx.node

                    ugen =
                        node.label

                    p =
                        setInput ( parIdx, val ) ugen.process

                    newNode =
                        { node | label = { ugen | process = p } }
                in
                { ugenCtx | node = newNode }
            )
        )
        graph


updateEdge : Int -> Float -> UGenGraph -> UGenGraph
updateEdge edgeId weight graph =
    Graph.mapEdges
        (\e ->
            if e.id == edgeId then
                { e | strength = weight }

            else
                e
        )
        graph


mulAllEdges : Float -> UGenGraph -> UGenGraph
mulAllEdges f graph =
    Graph.mapEdges
        (\e ->
            { e | strength = e.strength * f }
        )
        graph
