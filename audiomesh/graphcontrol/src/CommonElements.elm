module CommonElements exposing (simpleButton)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input


simpleButton : String -> msg -> Element msg
simpleButton s msg =
    Input.button [ Border.solid, Border.width 1, padding 10 ]
        { onPress = Just msg
        , label = text s
        }
