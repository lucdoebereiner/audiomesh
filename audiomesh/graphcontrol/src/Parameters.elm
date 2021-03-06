module Parameters exposing (..)


type Mapping
    = Lin
    | Exp


type alias Parameter =
    { idx : Int
    , value : Float
    , mapping : Mapping
    , name : String
    , min : Float
    , max : Float
    }



-- type alias ParameterSpec =
--     { value : Float
--     , mapping : Mapping
--     , min : Float
--     , max : Float
--     }


mapped : Parameter -> Float
mapped p =
    case p.mapping of
        Lin ->
            linlin p.value 0.0 1.0 p.min p.max

        Exp ->
            linexp p.value 0.0 1.0 p.min p.max



-- v assumed to be between 0 and 1


unmapped : Parameter -> Float -> Float
unmapped p v =
    case p.mapping of
        Lin ->
            linlin v p.min p.max 0.0 1.0

        Exp ->
            explin v p.min p.max 0.0 1.0



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
