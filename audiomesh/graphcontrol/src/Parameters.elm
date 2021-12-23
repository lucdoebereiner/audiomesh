module Parameters exposing (Mapping(..), Parameter, calcEdgeStrength, explin, lincubic, linexp, linlin, logE, mapped, softclip, tanh, uncubic, unmapped)


type Mapping
    = Lin
    | Exp
    | Cubic


type alias Parameter =
    { idx : Int
    , value : Float
    , mapping : Mapping
    , name : String
    , min : Float
    , max : Float
    }


mapped : Parameter -> Float
mapped p =
    case p.mapping of
        Lin ->
            linlin p.value 0.0 1.0 p.min p.max

        Exp ->
            if p.min == 0.0 then
                linexp p.value 0.0 1.0 (p.min + 0.001) p.max - 0.001

            else
                linexp p.value 0.0 1.0 p.min p.max

        Cubic ->
            lincubic p.value p.min p.max



-- v assumed to be between 0 and 1


unmapped : Parameter -> Float -> Float
unmapped p v =
    case p.mapping of
        Lin ->
            linlin v p.min p.max 0.0 1.0

        Exp ->
            if p.min == 0.0 then
                explin v (p.min + 0.001) p.max 0.0 1.0

            else
                explin v p.min p.max 0.0 1.0

        Cubic ->
            uncubic (linlin v p.min p.max -1.0 1.0)



-- par : Mapping -> Float -> Float -> Parameter
-- par =
--     Parameter 0.0


logE =
    logBase e


linlin : Float -> Float -> Float -> Float -> Float -> Float
linlin x a b c d =
    if x <= a then
        c

    else if x >= b then
        d

    else
        (x - a) / (b - a) * (d - c) + c


linexp : Float -> Float -> Float -> Float -> Float -> Float
linexp x a b c d =
    let
        c_shifted =
            if c == 0.0 then
                0.001

            else
                c
    in
    if x <= a then
        c

    else if x >= b then
        d

    else
        (d / c_shifted) ^ ((x - a) / (b - a)) * c_shifted


explin : Float -> Float -> Float -> Float -> Float -> Float
explin x a b c d =
    let
        a_shifted =
            if a == 0.0 then
                0.001

            else
                a
    in
    if x <= a then
        c

    else if x >= b then
        d

    else
        logE (x / a_shifted) / logE (b / a_shifted) * (d - c) + c


lincubic : Float -> Float -> Float -> Float
lincubic x a b =
    let
        cubic =
            ((x - 0.5) * 2.0) ^ 3
    in
    linlin cubic -1.0 1.0 a b


uncubic : Float -> Float
uncubic v =
    let
        vsign =
            if v < 0.0 then
                -1.0

            else
                1.0
    in
    ((abs v ^ (1.0 / 3.0)) * vsign) / 2.0 + 0.5


tanh : Float -> Float
tanh x =
    ((e ^ x) - (e ^ -x)) / ((e ^ x) + (e ^ -x))


softclip : Float -> Float
softclip x =
    let
        absx =
            abs x
    in
    if absx <= 0.5 then
        x

    else
        (absx - 0.25) / x


calcEdgeStrength : { bias : Float, factor : Float } -> Float
calcEdgeStrength strength =
    if strength.factor <= 1.0 then
        strength.bias * strength.factor

    else
        tanh (strength.bias * strength.factor * 0.5) * 2.0
