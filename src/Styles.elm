module Styles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


pageShell : Attribute msg
pageShell =
    css
        [ minHeight (vh 100)
        , backgroundColor (hex "F1F5F9")
        , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "sans-serif" ]
        , padding2 (rem 1.5) (rem 1)
        , color (hex "0F172A")
        ]


container : Attribute msg
container =
    css
        [ maxWidth (px 820)
        , margin2 zero auto
        ]


card : List Style -> Attribute msg
card extra =
    css
        ([ backgroundColor (hex "FFFFFF")
         , border3 (px 2) solid (hex "E2E8F0")
         , borderRadius (px 16)
         , padding (rem 1)
         ]
            ++ extra
        )


grid : Attribute msg
grid =
    css
        [ property "display" "grid"
        , property "grid-template-columns" "repeat(auto-fill, minmax(220px, 1fr))"
        , property "gap" "0.85rem"
        , marginBottom (rem 1.75)
        ]


row : Float -> Attribute msg
row gapRem =
    css
        [ displayFlex
        , alignItems center
        , flexWrap wrap
        , property "gap" (String.fromFloat gapRem ++ "rem")
        ]


pill : String -> String -> Attribute msg
pill bg fg =
    css
        [ fontSize (rem 0.75)
        , fontWeight (int 700)
        , padding2 (rem 0.2) (rem 0.6)
        , borderRadius (px 999)
        , backgroundColor (hex bg)
        , color (hex fg)
        ]


actionButton : Bool -> String -> Attribute msg
actionButton enabled bgHex =
    css
        [ padding2 (rem 0.6) (rem 1)
        , borderRadius (px 10)
        , border zero
        , fontWeight (int 700)
        , cursor
            (if enabled then
                pointer

             else
                notAllowed
            )
        , backgroundColor
            (if enabled then
                hex bgHex

             else
                hex "E2E8F0"
            )
        , color
            (if enabled then
                hex "FFFFFF"

             else
                hex "94A3B8"
            )
        ]
