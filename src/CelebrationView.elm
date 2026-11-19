module CelebrationView exposing (viewCelebration)

import Css exposing (..)
import Css.Animations as Anim
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Types exposing (..)


viewCelebration : Maybe Celebration -> Html FrontendMsg
viewCelebration maybeCel =
    case maybeCel of
        Nothing ->
            text ""

        Just cel ->
            div []
                [ viewConfettiLayer cel.id
                , viewToastBanner cel
                ]


viewToastBanner : Celebration -> Html FrontendMsg
viewToastBanner cel =
    div
        [ onClick (ClearCelebration cel.id)
        , css
            [ position fixed
            , top (rem 1.25)
            , left (pct 50)
            , transform (translateX (pct -50))
            , zIndex (int 1000)
            , backgroundColor
                (if cel.isReward then
                    hex "7C3AED"

                 else
                    hex "16A34A"
                )
            , color (hex "FFFFFF")
            , padding2 (rem 0.9) (rem 1.5)
            , borderRadius (px 18)
            , boxShadow4 zero (px 12) (px 28) (rgba 15 23 42 0.25)
            , textAlign center
            , cursor pointer
            , minWidth (px 260)
            ]
        ]
        [ div [ css [ fontSize (rem 1.25), fontWeight (int 800) ] ] [ text cel.title ]
        , div [ css [ fontSize (rem 0.9), opacity (num 0.95), marginTop (rem 0.2) ] ] [ text cel.subtitle ]
        ]


viewConfettiLayer : Int -> Html msg
viewConfettiLayer burstId =
    div
        [ css
            [ position fixed
            , top zero
            , left zero
            , width (vw 100)
            , height (vh 100)
            , pointerEvents none
            , overflow hidden
            , zIndex (int 999)
            ]
        ]
        (List.range 0 27 |> List.map (viewConfettiPiece burstId))


viewConfettiPiece : Int -> Int -> Html msg
viewConfettiPiece burstId idx =
    let
        colors =
            [ "F59E0B", "10B981", "3B82F6", "EC4899", "8B5CF6", "EF4444" ]

        pieceColor =
            colors
                |> List.drop (modBy 6 (idx + burstId))
                |> List.head
                |> Maybe.withDefault "F59E0B"

        leftPct =
            toFloat (modBy 96 (idx * 17 + 5))

        delaySec =
            toFloat (modBy 7 idx) * 0.08

        durationSec =
            1.8 + toFloat (modBy 5 idx) * 0.25

        fallAnim =
            Anim.keyframes
                [ ( 0, [ Anim.transform [ translateY (vh -5), rotate (deg 0) ], Anim.opacity (num 1) ] )
                , ( 100, [ Anim.transform [ translateY (vh 105), rotate (deg 540) ], Anim.opacity (num 0) ] )
                ]
    in
    span
        [ css
            [ position absolute
            , top zero
            , left (pct leftPct)
            , width (px 10)
            , height (px 16)
            , borderRadius (px 3)
            , backgroundColor (hex pieceColor)
            , animationName fallAnim
            , animationDuration (sec durationSec)
            , animationDelay (sec delaySec)
            , property "animation-fill-mode" "forwards"
            ]
        ]
        []
