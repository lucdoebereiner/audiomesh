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
    , mkGraph
    , mulAllEdges
    , processParameters
    , setInput
    , ugenLabel
    , updateEdge
    , updateProcessParameter
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
    | Ring
    | Filter { filterType : FilterType, freq : Float, q : Float }
    | Gauss
    | RMS
    | Sin { mul : Float }
    | SinOsc { freq : Float }
    | Constant { value : Float }
    | SoundIn { index : Int }
    | Square
    | BitNeg
    | BitOr
    | BitXOr
    | BitAnd
    | LinCon { linconA : Float, linconB : Float }



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

        SinOsc { freq } ->
            [ Parameter 1 freq Exp "Freq" 0.001 10000.0 ]

        Constant { value } ->
            [ Parameter 1 value Lin "Value" -1.0 1.0 ]

        Filter { filterType, freq, q } ->
            [ Parameter 1 freq Exp "Freq" 5.0 4000.0, Parameter 2 q Exp "Q" 0.1 20.0 ]

        LinCon { linconA, linconB } ->
            [ Parameter 1 linconA Exp "A" 0.001 10.0, Parameter 2 linconB Exp "B" 0.001 20.0 ]

        _ ->
            []


setInput : ( Int, Float ) -> Process -> Process
setInput ( parIdx, val ) proc =
    case proc of
        Mem _ ->
            Mem { lastValue = val }

        Sin _ ->
            Sin { mul = val }

        SinOsc _ ->
            SinOsc { freq = val }

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

        Mul ->
            "Mul"

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

        Add ->
            encObj p (JE.object [])

        Mul ->
            encObj p (JE.object [])

        Ring ->
            encObj p (JE.object [])

        Filter f ->
            encObj p
                (JE.object
                    [ ( "filter_type", JE.string (filterTypeToString f.filterType) )
                    , ( "freq", JE.float f.freq )
                    , ( "q", JE.float f.q )
                    ]
                )

        Gauss ->
            encObj p (JE.object [])

        RMS ->
            encObj p (JE.object [])

        Sin m ->
            encObj p (JE.object [ ( "mul", JE.float m.mul ) ])

        SinOsc s ->
            encObj p (JE.object [ ( "freq", JE.float s.freq ) ])

        Constant c ->
            encObj p (JE.object [ ( "value", JE.float c.value ) ])

        SoundIn s ->
            encObj p (JE.object [ ( "index", JE.int s.index ) ])

        Square ->
            encObj p (JE.object [])

        BitNeg ->
            encObj p (JE.object [])

        BitOr ->
            encObj p (JE.object [])

        BitXOr ->
            encObj p (JE.object [])

        BitAnd ->
            encObj p (JE.object [])

        LinCon l ->
            encObj p
                (JE.object
                    [ ( "lincon_a", JE.float l.linconA )
                    , ( "lincon_b", JE.float l.linconB )
                    ]
                )


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
            "SoundIn " ++ String.fromInt i.index

        SinOsc f ->
            "SinOsc " ++ floatString f.freq

        Sin s ->
            "Sin " ++ floatString s.mul

        LinCon l ->
            "LinCon a: "
                ++ floatString l.linconA
                ++ " b: "
                ++ floatString l.linconB

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


decodeAdd : Decoder Process
decodeAdd =
    Decode.succeed Add


decodeSoundIn : Decoder Process
decodeSoundIn =
    Decode.succeed (\input -> SoundIn { index = input })
        |> required "index" int


decodeMul : Decoder Process
decodeMul =
    Decode.succeed Mul


decodeRing : Decoder Process
decodeRing =
    Decode.succeed Ring


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


decodeRMS : Decoder Process
decodeRMS =
    Decode.succeed RMS


decodeSin : Decoder Process
decodeSin =
    Decode.succeed (\m -> Sin { mul = m })
        |> required "mul" float


decodeGauss : Decoder Process
decodeGauss =
    Decode.succeed Gauss


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
    Decode.succeed (\f -> SinOsc { freq = f })
        |> required "freq" float


decodeLinCon : Decoder Process
decodeLinCon =
    Decode.succeed (\a b -> LinCon { linconA = a, linconB = b })
        |> required "lincon_a" float
        |> required "lincon_b" float


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
    UGen Add True None


decodeUGen : Decoder UGen
decodeUGen =
    Decode.succeed (\p sum clip -> UGen p sum (clipTypeFromString clip))
        |> required "process"
            (oneOf
                [ field "Delay" decodeDelay
                , field "Add" decodeAdd
                , field "Mem" decodeMem
                , field "Mul" decodeMul
                , field "Ring" decodeRing
                , field "RMS" decodeRMS
                , field "Constant" decodeConstant
                , field "Sin" decodeSin
                , field "Gauss" decodeGauss
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
