module ProcessGraph exposing
    ( BackendGraph
    , ClipType(..)
    , Connection
    , FilterType(..)
    , Link
    , Process(..)
    , ProcessType(..)
    , UGen
    , UGenGraph
    , decodeGraph
    , defaultUGen
    , encodeProcess
    , getNodes
    , graphEdges
    , isOutputProcess
    , mkGraph
    , mulAllEdges
    , nextEdge
    , nextNode
    , prevEdge
    , prevNode
    , processParameters
    , setInput
    , ugenLabel
    , ugensWithIds
    , updateEdgeDelay
    , updateEdgeFreq
    , updateEdgeWeight
    , updateOutputAmp
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
        , at
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
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
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
    | VarDelay { delay : Float, maxdelay : Float }
    | Kaneko { e : Float, a : Float }
    | KanekoChain { n : Int, e : Float, a : Float }
    | GateDecision
        { min_dur_on : Float
        , max_dur_on : Float
        , min_dur_off : Float
        , max_dur_off : Float
        }
    | Add
    | Mul
    | Fold { threshold : Float, mul : Float, add : Float }
    | Softclip
    | Ducking
    | GateIfGreater
    | VanDerPol { e : Float, frac : Float, a : Float }
    | Duffing { e : Float, frac : Float, a : Float }
    | EnvFollow
    | Env
        { min_target : Float
        , max_target : Float
        , max_n : Float
        , sustain_fac : Float
        , rest_fac : Float
        }
    | PLL { factor : Float }
    | Ring
    | Filter { filterType : FilterType, freq : Float, q : Float }
    | Resonator { freqFactor : Float, freqCenter : Float, decay : Float }
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


isOutputProcess : Process -> Bool
isOutputProcess p =
    case p of
        Delay _ ->
            False

        -- VarDelay _ ->
        --     False
        RMS ->
            False

        SoundIn _ ->
            False

        _ ->
            True



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
            [ Parameter 1 mul Exp "Mul" 0.0 10.0 ]

        PLL { factor } ->
            [ Parameter 1 factor Exp "Fac" 0.05 2.0 ]

        SoundIn { index, factor } ->
            [ Parameter 1 factor Exp "Fac" 0.0 10.0 ]

        Kaneko { e, a } ->
            [ Parameter 2 e Lin "e" 0.0 1.0
            , Parameter 3 a Lin "a" 1.0 2.0
            ]

        KanekoChain { e, a } ->
            [ Parameter 2 e Lin "e" 0.0 1.0
            , Parameter 3 a Lin "a" 1.0 2.0
            ]

        GateDecision { min_dur_on, max_dur_on, min_dur_off, max_dur_off } ->
            [ Parameter 2 min_dur_on Exp "Min dur on" 0.05 10.0
            , Parameter 3 max_dur_on Exp "Max dur on" 0.2 20.0
            , Parameter 4 min_dur_off Exp "Min dur off" 0.05 10.0
            , Parameter 5 max_dur_off Exp "Max dur off" 0.2 20.0
            ]

        Fold { threshold, mul, add } ->
            [ Parameter 1 threshold Lin "Threshold" 0.0 1.0
            , Parameter 2 mul Exp "Mul" 1.0 20.0
            , Parameter 3 add Lin "Add" 0.0 1.0
            ]

        VarDelay { delay } ->
            [ Parameter 1 delay Exp "Delay" 0.0 7.0 ]

        SinOsc { freq, freq_mul } ->
            [ Parameter 1 freq Exp "Freq" 0.0 10000.0
            , Parameter 2 freq_mul Exp "Freq Mul" 0.1 10000.0
            ]

        VanDerPol { e, frac, a } ->
            [ Parameter 1 e Exp "E" 0.02 5
            , Parameter 2 frac Exp "dt" 0.001 0.5
            , Parameter 3 a Exp "a" 0.0001 30.0
            ]

        Duffing { e, frac, a } ->
            [ Parameter 1 e Exp "E" 0.05 20
            , Parameter 2 frac Exp "dt" 0.001 0.9
            , Parameter 3 a Exp "a" 0.001 10.0
            ]

        Env { min_target, max_target, max_n, sustain_fac, rest_fac } ->
            [ Parameter 1 min_target Exp "Min" 0.0 0.5
            , Parameter 2 max_target Exp "Max" 0.1 0.9
            , Parameter 3 max_n Exp "Sec" 0.5 20.0
            , Parameter 4 sustain_fac Exp "Sustain fac" 0.1 2.0
            , Parameter 5 rest_fac Exp "Rest fac" 0.0 1.0
            ]

        -- Constant { value } ->
        --     [ Parameter 1 value Lin "Value" -1.0 1.0 ]
        Filter { filterType, freq, q } ->
            [ Parameter 1 freq Exp "Freq" 10.0 5000.0
            , Parameter 2 q Exp "Q" 0.1 20.0
            ]

        Resonator { freqCenter, freqFactor, decay } ->
            [ Parameter 2 freqCenter Exp "Freq Center" 10.0 8000.0
            , Parameter 3 freqFactor Exp "Freq Fac" 1.0 8000.0
            , Parameter 4 decay Exp "Decay" 0.001 0.1
            ]

        LinCon { linconA, linconB } ->
            [ Parameter 1 linconA Exp "A" 0.001 10.0, Parameter 2 linconB Exp "B" 0.001 20.0 ]

        Compressor { threshold, ratio, makeup } ->
            [ Parameter 1 threshold Exp "Threshold" 0.01 0.9
            , Parameter 2 ratio Exp "Ratio" 1.0 20.0
            , Parameter 3 makeup Exp "Makup" 1.0 4.0
            ]

        Spike { threshold, tConst, r, tRest } ->
            [ Parameter 1 threshold Exp "Threshold" 0.01 0.9
            , Parameter 2 tConst Exp "T Const" 0.00001 0.3
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

        VarDelay d ->
            VarDelay { d | delay = val }

        Kaneko k ->
            case parIdx of
                2 ->
                    Kaneko { k | e = val }

                3 ->
                    Kaneko { k | a = val }

                _ ->
                    proc

        KanekoChain k ->
            case parIdx of
                2 ->
                    KanekoChain { k | e = val }

                3 ->
                    KanekoChain { k | a = val }

                _ ->
                    proc

        GateDecision g ->
            case parIdx of
                2 ->
                    GateDecision { g | min_dur_on = val }

                3 ->
                    GateDecision { g | max_dur_on = val }

                4 ->
                    GateDecision { g | min_dur_off = val }

                5 ->
                    GateDecision { g | max_dur_off = val }

                _ ->
                    proc

        Fold f ->
            case parIdx of
                1 ->
                    Fold { f | threshold = val }

                2 ->
                    Fold { f | mul = val }

                3 ->
                    Fold { f | add = val }

                _ ->
                    proc

        VanDerPol s ->
            case parIdx of
                1 ->
                    VanDerPol { s | e = val }

                2 ->
                    VanDerPol { s | frac = val }

                3 ->
                    VanDerPol { s | a = val }

                _ ->
                    proc

        Duffing s ->
            case parIdx of
                1 ->
                    Duffing { s | e = val }

                2 ->
                    Duffing { s | frac = val }

                3 ->
                    Duffing { s | a = val }

                _ ->
                    proc

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

        Env e ->
            case parIdx of
                1 ->
                    Env { e | min_target = val }

                2 ->
                    Env { e | max_target = val }

                3 ->
                    Env { e | max_n = val }

                4 ->
                    Env { e | sustain_fac = val }

                5 ->
                    Env { e | rest_fac = val }

                _ ->
                    proc

        Resonator r ->
            case parIdx of
                2 ->
                    Resonator { r | freqCenter = val }

                3 ->
                    Resonator { r | freqFactor = val }

                4 ->
                    Resonator { r | decay = val }

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

        GateIfGreater ->
            "GateIfGreater"

        GateDecision _ ->
            "GateDecision"

        Delay _ ->
            "Delay"

        VarDelay _ ->
            "VarDelay"

        Fold _ ->
            "Fold"

        Kaneko _ ->
            "Kaneko"

        KanekoChain _ ->
            "KanekoChain"

        Add ->
            "Add"

        VanDerPol _ ->
            "VanDerPol"

        Duffing _ ->
            "Duffing"

        Resonator _ ->
            "Resonator"

        PLL _ ->
            "PLL"

        Mul ->
            "Mul"

        Ducking ->
            "Ducking"

        EnvFollow ->
            "EnvFollow"

        Env _ ->
            "Env"

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

        VarDelay d ->
            encObj p
                (JE.object
                    [ ( "delay", JE.float d.delay )
                    , ( "maxdelay", JE.float d.maxdelay )
                    ]
                )

        Kaneko k ->
            encObj p
                (JE.object
                    [ ( "e", JE.float k.e )
                    , ( "a", JE.float k.a )
                    ]
                )

        KanekoChain k ->
            encObj p
                (JE.object
                    [ ( "e", JE.float k.e )
                    , ( "a", JE.float k.a )
                    , ( "last_outputs", JE.int k.n )
                    ]
                )

        GateDecision k ->
            encObj p
                (JE.object
                    [ ( "min_dur_on", JE.float k.min_dur_on )
                    , ( "max_dur_on", JE.float k.max_dur_on )
                    , ( "min_dur_off", JE.float k.min_dur_off )
                    , ( "max_dur_off", JE.float k.max_dur_off )
                    ]
                )

        Fold f ->
            encObj p
                (JE.object
                    [ ( "threshold", JE.float f.threshold )
                    , ( "mul", JE.float f.mul )
                    , ( "add", JE.float f.add )
                    ]
                )

        Env e ->
            encObj p
                (JE.object
                    [ ( "min_target", JE.float e.min_target )
                    , ( "max_target", JE.float e.max_target )
                    , ( "max_n", JE.float e.max_n )
                    , ( "sustain_fac", JE.float e.sustain_fac )
                    , ( "rest_fac", JE.float e.rest_fac )
                    ]
                )

        Resonator r ->
            encObj p
                (JE.object
                    [ ( "freq_center", JE.float r.freqCenter )
                    , ( "freq_factor", JE.float r.freqFactor )
                    , ( "decay", JE.float r.decay )
                    ]
                )

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

        VanDerPol s ->
            encObj p
                (JE.object
                    [ ( "e", JE.float s.e )
                    , ( "frac", JE.float s.frac )
                    , ( "a", JE.float s.a )
                    ]
                )

        Duffing s ->
            encObj p
                (JE.object
                    [ ( "e", JE.float s.e )
                    , ( "frac", JE.float s.frac )
                    , ( "a", JE.float s.a )
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

        KanekoChain k ->
            "KanekoChain n:" ++ String.fromInt k.n

        VarDelay d ->
            "VarDelay d:" ++ floatString d.delay

        Fold f ->
            "Fold thresh:" ++ floatString f.threshold

        VanDerPol v ->
            "VanDerPol e:" ++ String.fromFloat v.e

        Duffing v ->
            "Duffing e:" ++ String.fromFloat v.e

        Filter f ->
            filterTypeToString f.filterType
                ++ " freq:"
                ++ floatString f.freq
                ++ " q:"
                ++ floatString f.q

        Resonator r ->
            "Resonator"
                --                ++ floatString r.freq
                ++ " decay:"
                ++ floatString r.decay

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


decodeVarDelay : Decoder Process
decodeVarDelay =
    Decode.succeed (\d m -> VarDelay { delay = d, maxdelay = m })
        |> required "delay" float
        |> required "maxdelay" float


decodeGateDecision : Decoder Process
decodeGateDecision =
    Decode.succeed
        (\mi mx mi_off mx_off ->
            GateDecision
                { min_dur_on = mi
                , max_dur_on = mx
                , min_dur_off = mi_off
                , max_dur_off = mx_off
                }
        )
        |> required "min_dur_on" float
        |> required "max_dur_on" float
        |> required "min_dur_off" float
        |> required "max_dur_off" float


decodeKaneko : Decoder Process
decodeKaneko =
    Decode.succeed (\e a -> Kaneko { e = e, a = a })
        |> required "e" float
        |> required "a" float


decodeKanekoChain : Decoder Process
decodeKanekoChain =
    Decode.succeed (\e a n -> KanekoChain { n = n, e = e, a = a })
        |> required "e" float
        |> required "a" float
        |> required "last_outputs" int


decodeSoundIn : Decoder Process
decodeSoundIn =
    Decode.succeed (\input f -> SoundIn { index = input, factor = f })
        |> required "index" int
        |> required "factor" float


decodeFold : Decoder Process
decodeFold =
    Decode.succeed
        (\t m a ->
            Fold
                { threshold = t
                , mul = m
                , add = a
                }
        )
        |> required "threshold" float
        |> required "mul" float
        |> required "add" float


decodeVanDerPol : Decoder Process
decodeVanDerPol =
    Decode.succeed (\e f a -> VanDerPol { e = e, frac = f, a = a })
        |> required "e" float
        |> required "frac" float
        |> required "a" float


decodeDuffing : Decoder Process
decodeDuffing =
    Decode.succeed (\e f a -> Duffing { e = e, frac = f, a = a })
        |> required "e" float
        |> required "frac" float
        |> required "a" float


decodeEnv : Decoder Process
decodeEnv =
    Decode.succeed
        (\min max n sus rest ->
            Env
                { min_target = min
                , max_target = max
                , max_n = n
                , sustain_fac = sus
                , rest_fac = rest
                }
        )
        |> required "min_target" float
        |> required "max_target" float
        |> required "max_n" float
        |> required "sustain_fac" float
        |> required "rest_fac" float


decodeSimpleProcess : Process -> Decoder Process
decodeSimpleProcess p =
    Decode.succeed p


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


decodeResonator : Decoder Process
decodeResonator =
    Decode.succeed
        (\c f d ->
            Resonator
                { freqCenter = c
                , freqFactor = f
                , decay = d
                }
        )
        |> required "freq_center" float
        |> required "freq_factor" float
        |> required "decay" float


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


type ProcessType
    = NoInputGenerator
    | TwoInputs
    | MultipleInputs
    | TransparentProcessor
    | OpaqueProcessor
    | SidechainEnv


processTypeFromString : String -> ProcessType
processTypeFromString s =
    case s of
        "NoInputGenerator" ->
            NoInputGenerator

        "TransparentProcessor" ->
            TransparentProcessor

        "OpaqueProcessor" ->
            OpaqueProcessor

        "SidechainEnv" ->
            SidechainEnv

        "TwoInputs" ->
            TwoInputs

        "MultipleInputs" ->
            MultipleInputs

        _ ->
            OpaqueProcessor


type alias UGen =
    { process : Process
    , sum_inputs : Bool
    , clip : ClipType
    , output_sends : IntDict Float
    , process_type : ProcessType
    , output_amp : Float
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
    UGen Add True None IntDict.empty OpaqueProcessor 1.0


decodeOutputTuple =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.float)


decodeUGen : Decoder UGen
decodeUGen =
    Decode.succeed
        (\p sum clip sends pt a ->
            UGen p
                sum
                (clipTypeFromString clip)
                (IntDict.fromList sends)
                (processTypeFromString pt)
                a
        )
        |> required "process"
            (oneOf
                [ field "Delay" decodeDelay
                , field "VarDelay" decodeVarDelay
                , field "Add" (decodeSimpleProcess Add)
                , field "Mem" decodeMem
                , field "Mul" (decodeSimpleProcess Mul)
                , field "Softclip" (decodeSimpleProcess Softclip)
                , field "Ring" (decodeSimpleProcess Ring)
                , field "RMS" (decodeSimpleProcess RMS)
                , field "Env" decodeEnv
                , field "Constant" decodeConstant
                , field "Compressor" decodeCompressor
                , field "Spike" decodeSpike
                , field "Sin" decodeSin
                , field "Fold" decodeFold
                , field "Ducking" (decodeSimpleProcess Ducking)
                , field "Gauss" (decodeSimpleProcess Gauss)
                , field "EnvFollow" (decodeSimpleProcess EnvFollow)
                , field "PLL" decodePLL
                , field "VanDerPol" decodeVanDerPol
                , field "Duffing" decodeDuffing
                , field "Resonator" decodeResonator
                , field "SoundIn" decodeSoundIn
                , field "Filter" decodeFilter
                , field "Kaneko" decodeKaneko
                , field "KanekoChain" decodeKanekoChain
                , field "GateDecision" decodeGateDecision
                , field "SinOsc" decodeSinOsc
                , field "LinCon" decodeLinCon
                , field "GateIfGreater" (decodeSimpleProcess GateIfGreater)
                , field "Square" (decodeSimpleProcess Square)
                , field "BitAnd" decodeBitAnd
                , field "BitOr" decodeBitOr
                , field "BitXOr" decodeBitXOr
                , field "BitNeg" decodeBitNeg
                ]
            )
        |> required "sum_inputs" bool
        |> required "clip" string
        |> required "output_sends" (list decodeOutputTuple)
        |> required "process_type" string
        |> required "output_amp" float


type alias Link =
    { index : Int
    , strength : Float
    , delay : { delay : Float, maxdelay : Float }
    , id : Int
    , freq : Float
    }


type alias Connection =
    { from : Int
    , to : Int
    , link : Link
    }


type IntOrLink
    = IntElement Int
    | LinkObj Link


decodeIntElement : Decoder IntOrLink
decodeIntElement =
    Decode.map IntElement int


decodeLinkObj : Decoder Link
decodeLinkObj =
    Decode.succeed
        (\idx w d m f ->
            Link idx w { delay = d, maxdelay = m } -1 f
        )
        |> required "input_idx" int
        |> required "weight" float
        |> custom (at [ "delay", "delay" ] float)
        |> custom (at [ "delay", "maxdelay" ] float)
        |> required "output" float



-- decodeFloatArray : Decoder IntOrFloatArray
-- decodeFloatArray =
--     Decode.map FloatArray (array float)


decodeLink : Decoder (Array IntOrLink)
decodeLink =
    array (oneOf [ Decode.map LinkObj decodeLinkObj, decodeIntElement ])


decodeConnection : Decoder (Maybe (Array IntOrLink))
decodeConnection =
    nullable decodeLink


connectionFromArray : Array IntOrLink -> Int -> Maybe Connection
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

        linkObjM =
            Maybe.andThen
                (\a ->
                    case a of
                        LinkObj l ->
                            Just l

                        _ ->
                            Nothing
                )
                (Array.get 2 ar)
    in
    case ( fromM, toM, linkObjM ) of
        ( Just from, Just to, Just linkObj ) ->
            Just <| Connection from to { linkObj | id = id }

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


connectionsFromArrays : List (Maybe (Array IntOrLink)) -> Int -> List Connection
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


graphEdges : UGenGraph -> List Connection
graphEdges gr =
    List.map
        (\e ->
            { from = e.from
            , to = e.to
            , link = e.label
            }
        )
        (Graph.edges
            gr
        )


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


updateOutputAmp : Graph.NodeId -> Float -> UGenGraph -> UGenGraph
updateOutputAmp nodeId amp graph =
    Graph.update nodeId
        (Maybe.map
            (\ctx ->
                let
                    node =
                        ctx.node

                    ugen =
                        node.label
                in
                { ctx | node = { node | label = { ugen | output_amp = amp } } }
            )
        )
        graph


updateEdgeWeight : Int -> Float -> UGenGraph -> UGenGraph
updateEdgeWeight edgeId weight graph =
    Graph.mapEdges
        (\e ->
            if e.id == edgeId then
                { e | strength = weight }

            else
                e
        )
        graph


updateEdgeFreq : Int -> Float -> UGenGraph -> UGenGraph
updateEdgeFreq edgeId freq graph =
    Graph.mapEdges
        (\e ->
            if e.id == edgeId then
                { e | freq = freq }

            else
                e
        )
        graph


updateEdgeDelay : Int -> Float -> UGenGraph -> UGenGraph
updateEdgeDelay edgeId d graph =
    Graph.mapEdges
        (\e ->
            if e.id == edgeId then
                let
                    delay =
                        e.delay

                    newDelay =
                        { delay | delay = d }
                in
                { e | delay = newDelay }

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
