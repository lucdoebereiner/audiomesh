module Utils exposing (..)


floatString : Float -> String
floatString f =
    round (f * 1000)
        |> toFloat
        |> (\fl -> fl / 1000.0)
        |> String.fromFloat
