module ProcessGraph exposing
    ( BackendGraph
    , ClipType(..)
    , Connection
    , FilterType(..)
    ,  InputRange
       -- , InputScaling

    , InputSpec
    , InputType
    ,  Link
       -- , Process(..)

    , ProcessSpec
    , ProcessState
    , ProcessType(..)
    , UGen
    , UGenGraph
    , decodeGraph
    , decodeProcessSpec
    ,  defaultUGen
       -- , encodeProcess

    , encodeProcessState
    , getEdge
    , getNodes
    , graphEdges
    , isOutputProcess
    ,  mkGraph
       --    , mulAllEdges

    , nextEdge
    , nextNode
    , prevEdge
    , prevNode
    , processParameters
    , processSpecToState
    ,  processStateDisplay
       -- , setInput

    , ugenLabel
    , ugensWithIds
    , updateEdgeBias
    , updateEdgeDelay
    , updateEdgeFactor
    , updateEdgeFreq
    , updateInputState
    , updateOutputAmp
    , updateProcessParameter
    , upsertOutputSend
    )

import Array exposing (Array)
import CommonElements exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
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
import Maybe.Extra as M
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


isOutputProcess : ProcessState -> Bool
isOutputProcess p =
    case p.name of
        "Delay" ->
            False

        -- VarDelay _ ->
        --     False
        "RMS" ->
            False

        "SoundIn" ->
            False

        _ ->
            True


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
    { process : ProcessState
    , sum_inputs : Bool
    , clip : ClipType
    , output_sends : IntDict Float
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
    u.process.name


defaultUGen : UGen
defaultUGen =
    UGen { name = "Add", processType = TransparentProcessor, inputs = [] } True None IntDict.empty 1.0



--UGen Add True None IntDict.empty OpaqueProcessor 1.0


decodeOutputTuple =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.float)


decodeUGen : List ProcessSpec -> Decoder UGen
decodeUGen specs =
    Decode.succeed
        (\p sum clip sends a ->
            UGen p
                sum
                (clipTypeFromString clip)
                (IntDict.fromList sends)
                a
        )
        |> required "process" (decodeProcessState specs)
        |> required "sum_inputs" bool
        |> required "clip" string
        |> required "output_sends" (list decodeOutputTuple)
        |> required "output_amp" float


type alias Link =
    { index : Int
    , strength : { bias : Float, factor : Float }
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
            Link idx { bias = w, factor = 1.0 } { delay = d, maxdelay = m } -1 f
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


decodeGraph : List ProcessSpec -> Decoder BackendGraph
decodeGraph specs =
    Decode.succeed (\n nh e outs -> BackendGraph n nh (connectionsFromArrays e 0) outs)
        |> required "nodes" (list (decodeUGen specs))
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
                        updateInputState parIdx (String.fromFloat val) ugen.process

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


getEdge : Int -> UGenGraph -> Maybe Link
getEdge edgeId graph =
    Graph.edges graph |> L.find (\e -> e.label.id == edgeId) |> Maybe.map .label


updateEdgeBias : Int -> Float -> UGenGraph -> UGenGraph
updateEdgeBias edgeId weight graph =
    Graph.mapEdges
        (\e ->
            if e.id == edgeId then
                let
                    s =
                        e.strength
                in
                { e | strength = { s | bias = weight } }

            else
                e
        )
        graph


updateEdgeFactor : Int -> Float -> UGenGraph -> UGenGraph
updateEdgeFactor edgeId factor graph =
    Graph.mapEdges
        (\e ->
            if e.id == edgeId then
                let
                    s =
                        e.strength
                in
                { e | strength = { s | factor = factor } }

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



-- mulAllEdges : Float -> UGenGraph -> UGenGraph
-- mulAllEdges f graph =
--     Graph.mapEdges
--         (\e ->
--             { e | strength = e.strength * f }
--         )
--         graph


type alias ProcessState =
    { name : String
    , processType : ProcessType
    , inputs : List InputState
    }


updateInputState : Int -> String -> ProcessState -> ProcessState
updateInputState idx val state =
    { name = state.name
    , processType = state.processType
    , inputs = L.updateIf (\i -> i.index == idx) (\i -> { i | state = val }) state.inputs
    }


updateInputStateByName : String -> String -> ProcessState -> ProcessState
updateInputStateByName inputName val state =
    { name = state.name
    , processType = state.processType
    , inputs = L.updateIf (\i -> i.name == inputName) (\i -> { i | state = val }) state.inputs
    }


updateFromKeyValList : List ( String, String ) -> ProcessState -> ProcessState
updateFromKeyValList values state =
    List.foldl (\( inputName, value ) currState -> updateInputStateByName inputName value currState) state values


inputStateField : (Int -> String -> msg) -> InputState -> Element msg
inputStateField setMsg state =
    Input.text [ width (px 80) ]
        { onChange = setMsg state.index
        , text = state.state
        , placeholder = Nothing
        , label = Input.labelLeft [] (text state.name)
        }


processStateDisplay : ProcessState -> (Int -> String -> msg) -> (ProcessState -> msg) -> Element msg
processStateDisplay state setInputMsg addProcessMsg =
    let
        button =
            encodeProcessState state
                |> Maybe.map
                    (\v ->
                        simpleButton state.name (addProcessMsg state)
                    )
                |> Maybe.withDefault none
    in
    row
        [ spacing 5
        , Border.solid
        , Border.width
            (if List.isEmpty state.inputs then
                0

             else
                1
            )
        ]
        (List.map (inputStateField setInputMsg) state.inputs ++ [ button ])



-- sinOscInput : String -> String -> Element Msg
-- sinOscInput fr frm =
--     row [ spacing 5, Border.solid, Border.width 1 ]
--         [ Input.text [ width (px 80) ]
--             { onChange = SetSinOscFreq
--             , text = fr
--             , placeholder = Nothing
--             , label = Input.labelLeft [] (text "Freq")
--             }
--         , Input.text [ width (px 80) ]
--             { onChange = SetSinOscFreqMul
--             , text = frm
--             , placeholder = Nothing
--             , label = Input.labelLeft [] (text "Freq Mul")
--             }
--         , Maybe.withDefault none <|
--             Maybe.map2 (\ff fmf -> addProcess "SinOsc" (SinOsc { freq = ff, freq_mul = fmf })) (String.toFloat fr) (String.toFloat frm)
--         ]


encodeProcessState : ProcessState -> Maybe Value
encodeProcessState state =
    List.map encodeInputState state.inputs
        |> M.combine
        |> Maybe.map
            (\is ->
                JE.object
                    [ ( state.name
                      , JE.object is
                      )
                    ]
            )


stateFromPairs : List ProcessSpec -> List ( String, List ( String, Float ) ) -> Maybe ProcessState
stateFromPairs specs pairsLst =
    case pairsLst of
        [ ( name, inputs ) ] ->
            L.find (\s -> s.name == name) specs
                |> Maybe.map (\s -> updateFromKeyValList (List.map (\( n, f ) -> ( n, String.fromFloat f )) inputs) (processSpecToState s))

        _ ->
            Nothing


decodeProcessState : List ProcessSpec -> Decoder ProcessState
decodeProcessState specs =
    Decode.andThen
        (\p ->
            case stateFromPairs specs p of
                Just s ->
                    Decode.succeed s

                _ ->
                    Decode.fail "Invalid process state: "
        )
        (Decode.keyValuePairs (Decode.keyValuePairs float))



-- Decode.succeed
-- (\name processType inputs ->
--     ProcessSpec name processType inputs
-- )
-- |> required "name" string
-- |> required "process_type" processTypeDecoder
-- |> required "inputs" (list decodeInputSpec)


type alias ProcessSpec =
    { name : String
    , processType : ProcessType
    , inputs : List InputSpec
    }


decodeProcessSpec : Decoder ProcessSpec
decodeProcessSpec =
    Decode.succeed
        (\name processType inputs ->
            ProcessSpec name processType inputs
        )
        |> required "name" string
        |> required "process_type" processTypeDecoder
        |> required "inputs" (list decodeInputSpec)


processTypeFromStringDec : String -> Decoder ProcessType
processTypeFromStringDec s =
    case s of
        "NoInputGenerator" ->
            Decode.succeed NoInputGenerator

        "TransparentProcessor" ->
            Decode.succeed TransparentProcessor

        "OpaqueProcessor" ->
            Decode.succeed OpaqueProcessor

        "SidechainEnv" ->
            Decode.succeed SidechainEnv

        "TwoInputs" ->
            Decode.succeed TwoInputs

        "MultipleInputs" ->
            Decode.succeed MultipleInputs

        _ ->
            Decode.fail ("Invalid process type: " ++ s)


processTypeDecoder : Decoder ProcessType
processTypeDecoder =
    Decode.string |> Decode.andThen processTypeFromStringDec


type alias InputSpec =
    { index : Int
    , name : String
    , inputType : InputType
    , controllable : Bool
    }


type alias InputState =
    { index : Int
    , name : String
    , inputType : InputType
    , controllable : Bool
    , state : String
    }


inputStateToParameter : InputState -> Parameter
inputStateToParameter st =
    { idx = st.index
    , name = st.name
    , value = Maybe.withDefault 0.0 (String.toFloat st.state)
    , mapping = inputTypeMapping st.inputType
    , min = inputTypeMin st.inputType
    , max = inputTypeMax st.inputType
    }


processParameters : ProcessState -> List Parameter
processParameters process =
    List.map inputStateToParameter (List.filter .controllable process.inputs)


encodeInputState : InputState -> Maybe ( String, Value )
encodeInputState is =
    let
        val =
            case is.inputType of
                Audio ->
                    Just JE.null

                Any _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Phase _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Q _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Amplitude _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Seconds _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Index _ ->
                    Maybe.map JE.int (String.toInt is.state)

                Samples _ ->
                    Maybe.map JE.int (String.toInt is.state)

                Frequency _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Factor _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Threshold _ ->
                    Maybe.map JE.float (String.toFloat is.state)

                Offset _ ->
                    Maybe.map JE.float (String.toFloat is.state)
    in
    Maybe.map (\v -> ( is.name, v )) val


processSpecToState : ProcessSpec -> ProcessState
processSpecToState spec =
    { name = spec.name
    , processType = spec.processType
    , inputs = List.map stateFromSpec spec.inputs
    }


stateFromSpec : InputSpec -> InputState
stateFromSpec spec =
    { index = spec.index
    , name = spec.name
    , inputType = spec.inputType
    , controllable = spec.controllable
    , state = inputTypeDefaultString spec.inputType
    }


decodeInputSpec : Decoder InputSpec
decodeInputSpec =
    Decode.succeed
        (\index name inputType controllable ->
            InputSpec index name inputType controllable
        )
        |> required "index" int
        |> required "name" string
        |> required "input_type" inputTypeDecoder
        |> required "controllable" bool


type alias InputRange =
    { min : Float
    , max : Float
    , default : Float
    , scaling : Mapping
    }


decodeInputRange : Decoder InputRange
decodeInputRange =
    Decode.succeed (\mn mx def scal -> InputRange mn mx def scal)
        |> required "min" float
        |> required "max" float
        |> required "default" float
        |> required "scaling" inputScalingDecoder


inputScalingDecoder : Decoder Mapping
inputScalingDecoder =
    Decode.string |> Decode.andThen inputScalingFromString


inputScalingFromString : String -> Decoder Mapping
inputScalingFromString string =
    case string of
        -- "Log" ->
        --     Decode.succeed InputLog
        "Exp" ->
            Decode.succeed Exp

        "Cubic" ->
            Decode.succeed Cubic

        "Lin" ->
            Decode.succeed Lin

        _ ->
            Decode.fail ("Invalid input scaling: " ++ string)


type InputType
    = Any Float
    | Audio
    | Frequency InputRange
    | Q Float
    | Phase Float
    | Index Int
    | Factor InputRange
    | Threshold InputRange
    | Amplitude Float
    | Seconds Float
    | Offset InputRange
    | Samples Int


inputTypeMapping : InputType -> Mapping
inputTypeMapping t =
    case t of
        Any _ ->
            Lin

        Audio ->
            Lin

        Frequency r ->
            r.scaling

        Q _ ->
            Exp

        Phase _ ->
            Lin

        Index _ ->
            Lin

        Factor r ->
            r.scaling

        Threshold r ->
            r.scaling

        Amplitude f ->
            Cubic

        Seconds f ->
            Exp

        Offset r ->
            Lin

        Samples i ->
            Lin


inputTypeMin : InputType -> Float
inputTypeMin t =
    case t of
        Any _ ->
            -20000.0

        Audio ->
            -20000.0

        Frequency r ->
            r.min

        Q _ ->
            0.00001

        Phase _ ->
            -1000.0

        Index _ ->
            0.0

        Factor r ->
            r.min

        Threshold r ->
            r.min

        Amplitude f ->
            -10.0

        Seconds f ->
            0.0

        Offset r ->
            r.min

        Samples i ->
            0.0


inputTypeMax : InputType -> Float
inputTypeMax t =
    case t of
        Any _ ->
            20000.0

        Audio ->
            20000.0

        Frequency r ->
            r.max

        Q _ ->
            1000.0

        Phase _ ->
            1000.0

        Index _ ->
            100.0

        Factor r ->
            r.max

        Threshold r ->
            r.max

        Amplitude f ->
            10.0

        Seconds f ->
            10.0

        Offset r ->
            r.max

        Samples i ->
            100000.0


inputTypeDefaultString : InputType -> String
inputTypeDefaultString t =
    case t of
        Any f ->
            String.fromFloat f

        Audio ->
            ""

        Frequency r ->
            String.fromFloat r.default

        Q f ->
            String.fromFloat f

        Phase f ->
            String.fromFloat f

        Index i ->
            String.fromInt i

        Factor r ->
            String.fromFloat r.default

        Threshold r ->
            String.fromFloat r.default

        Amplitude f ->
            String.fromFloat f

        Seconds f ->
            String.fromFloat f

        Offset r ->
            String.fromFloat r.default

        Samples i ->
            String.fromInt i


inputTypeDecoder : Decoder InputType
inputTypeDecoder =
    oneOf
        [ Decode.map Any <| field "Any" float
        , Decode.map Q <| field "Q" float
        , Decode.map Phase <| field "Phase" float
        , Decode.map Index <| field "Index" int
        , Decode.map Amplitude <| field "Amplitude" float
        , Decode.map Seconds <| field "Seconds" float
        , Decode.map Samples <| field "Samples" int
        , Decode.map Frequency <| field "Frequency" decodeInputRange
        , Decode.map Factor <| field "Factor" decodeInputRange
        , Decode.map Threshold <| field "Threshold" decodeInputRange
        , Decode.map Offset <| field "Offset" decodeInputRange
        ]
