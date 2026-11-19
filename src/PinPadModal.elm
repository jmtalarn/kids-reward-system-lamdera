module PinPadModal exposing (viewPinPadModal)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import I18n
import Types exposing (..)


viewPinPadModal : FrontendModel -> Html FrontendMsg
viewPinPadModal model =
    if not model.showPinPad then
        text ""

    else
        div
            [ css
                [ position fixed
                , top zero
                , left zero
                , width (vw 100)
                , height (vh 100)
                , backgroundColor (rgba 15 23 42 0.55)
                , displayFlex
                , alignItems center
                , justifyContent center
                , zIndex (int 1100)
                , padding (rem 1)
                ]
            ]
            [ div
                [ css
                    [ backgroundColor (hex "FFFFFF")
                    , borderRadius (px 24)
                    , padding (rem 1.75)
                    , width (pct 100)
                    , maxWidth (px 320)
                    , textAlign center
                    , boxShadow4 zero (px 20) (px 40) (rgba 15 23 42 0.25)
                    ]
                ]
                [                  div [ css [ fontSize (rem 2.25), marginBottom (rem 0.25) ] ] [ text "🔐" ]
                , h3 [ css [ marginTop zero, marginBottom (rem 0.25), color (hex "0F172A") ] ] [ text (I18n.parentPinTitle model.lang) ]
                , p [ css [ marginTop zero, marginBottom (rem 1.25), fontSize (rem 0.85), color (hex "64748B") ] ] [ text (I18n.pinInstructions model.lang) ]
                , viewPinDots model.pinLength (String.length model.pinInput)
                , case model.errorMessage of
                    Just err ->
                        div
                            [ css
                                [ backgroundColor (hex "FEE2E2")
                                , color (hex "991B1B")
                                , fontSize (rem 0.85)
                                , fontWeight (int 600)
                                , padding2 (rem 0.45) (rem 0.75)
                                , borderRadius (px 10)
                                , marginBottom (rem 1)
                                ]
                            ]
                            [ text err ]

                    Nothing ->
                        text ""
                , viewKeypad (not (String.isEmpty model.pinInput)) model.lang
                ]
            ]


viewPinDots : Int -> Int -> Html msg
viewPinDots expectedLength enteredCount =
    let
        totalSlots =
            max 1 expectedLength
    in
    div [ css [ displayFlex, justifyContent center, property "gap" "0.6rem", marginBottom (rem 1.5), minHeight (px 18) ] ]
        (List.range 1 totalSlots
            |> List.map
                (\idx ->
                    let
                        filled =
                            idx <= enteredCount
                    in
                    span
                        [ css
                            [ width (px 14)
                            , height (px 14)
                            , borderRadius (pct 50)
                            , backgroundColor
                                (if filled then
                                    hex "2563EB"

                                 else
                                    hex "E2E8F0"
                                )
                            ]
                        ]
                        []
                )
        )


viewKeypad : Bool -> Language -> Html FrontendMsg
viewKeypad hasDigits lang =
    div []
        [ div
            [ css
                [ property "display" "grid"
                , property "grid-template-columns" "repeat(3, 1fr)"
                , property "gap" "0.65rem"
                , marginBottom (rem 0.75)
                ]
            ]
            (List.map viewDigitKey [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
                ++ [ viewControlKey "✕" ClosePinPad "FEE2E2" "991B1B"
                   , viewDigitKey "0"
                   , viewControlKey "⌫" BackspacePin "F1F5F9" "475569"
                   ]
            )
        , button
            [ onClick SubmitPin
            , Html.Styled.Attributes.disabled (not hasDigits)
            , css
                [ width (pct 100)
                , padding (rem 0.85)
                , borderRadius (px 14)
                , border zero
                , fontSize (rem 1.05)
                , fontWeight (int 700)
                , cursor
                    (if hasDigits then
                        pointer

                     else
                        notAllowed
                    )
                , backgroundColor
                    (if hasDigits then
                        hex "16A34A"

                     else
                        hex "E2E8F0"
                    )
                , color
                    (if hasDigits then
                        hex "FFFFFF"

                     else
                        hex "94A3B8"
                    )
                ]
            ]
            [ text (I18n.unlockButton lang) ]
        ]


viewDigitKey : String -> Html FrontendMsg
viewDigitKey digit =
    button
        [ onClick (PressPinDigit digit)
        , css
            [ padding (rem 0.9)
            , fontSize (rem 1.35)
            , fontWeight (int 700)
            , borderRadius (px 14)
            , border3 (px 1) solid (hex "E2E8F0")
            , backgroundColor (hex "F8FAFC")
            , color (hex "0F172A")
            , cursor pointer
            ]
        ]
        [ text digit ]


viewControlKey : String -> FrontendMsg -> String -> String -> Html FrontendMsg
viewControlKey labelText msg bgHex fgHex =
    button
        [ onClick msg
        , css
            [ padding (rem 0.9)
            , fontSize (rem 1.1)
            , fontWeight (int 700)
            , borderRadius (px 14)
            , border zero
            , backgroundColor (hex bgHex)
            , color (hex fgHex)
            , cursor pointer
            ]
        ]
        [ text labelText ]
