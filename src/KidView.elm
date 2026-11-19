module KidView exposing (viewHeroBanner, viewLedger, viewParticipantBar, viewRewardShop, viewTaskGrid)


import Css exposing (..)
import Dict
import Domain exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Styles
import Types exposing (..)
import I18n

viewParticipantBar : FrontendModel -> Html FrontendMsg
viewParticipantBar model =
    div [ Styles.row 0.75, css [ marginBottom (rem 1.5) ] ]
        (Dict.values model.participants
            |> List.map
                (\p ->
                    let
                        isSelected =
                            model.selectedParticipant == Just p.id

                        pts =
                            balanceFor p.id model.transactions
                    in
                    button
                        [ onClick (SelectParticipant (Just p.id))
                        , Styles.row 0.75
                        , css
                            [ padding2 (rem 0.75) (rem 1.25)
                            , borderRadius (px 16)
                            , cursor pointer
                            , fontSize (rem 1)
                            , fontWeight (int 700)
                            , border3 (px 3)
                                solid
                                (if isSelected then
                                    hex (String.dropLeft 1 p.color)

                                 else
                                    hex "E2E8F0"
                                )
                            , backgroundColor
                                (if isSelected then
                                    hex "FFFFFF"

                                 else
                                    hex "F8FAFC"
                                )
                            ]
                        ]
                        [ span [ css [ fontSize (rem 1.75) ] ] [ text p.avatar ]
                        , div [ css [ textAlign left ] ]
                            [ div [] [ text p.name ]
                            , div [ css [ fontSize (rem 0.85), color (hex (String.dropLeft 1 p.color)) ] ]
                                [ text ("⭐ " ++ String.fromInt pts ++ " pts") ]
                            ]
                        ]
                )
        )


-- viewKidDashboard : Participant -> FrontendModel -> Html FrontendMsg
-- viewKidDashboard kid model =
--     let
--         pts =
--             balanceFor kid.id model.transactions
--     in
--     div []
--         [ viewHeroBanner kid pts
--         , viewTaskGrid kid model
--         , viewRewardShop kid pts model
--         ]


viewHeroBanner : Language ->Participant -> Int -> Html FrontendMsg
viewHeroBanner lang kid points =
    div
        [ css
            [ backgroundColor (hex (String.dropLeft 1 kid.color))
            , color (hex "FFFFFF")
            , padding (rem 1.5)
            , borderRadius (px 20)
            , marginBottom (rem 1.75)
            , displayFlex
            , justifyContent spaceBetween
            , alignItems center
            ]
        ]
        [ div [ Styles.row 1 ]
            [ span [ css [ fontSize (rem 3) ] ] [ text kid.avatar ]
            , div []
                [ div [ css [ fontSize (rem 0.9), opacity (num 0.9) ] ] [ text <| I18n.welcomeBack lang ]
                , h2 [ css [ margin zero, fontSize (rem 1.75) ] ] [ text kid.name ]
                ]
            ]
        , div [ css [ backgroundColor (rgba 255 255 255 0.2), padding2 (rem 0.75) (rem 1.25), borderRadius (px 14), textAlign center ] ]
            [ div [ css [ fontSize (rem 1.75), fontWeight (int 800) ] ] [ text (String.fromInt points) ]
            , div [ css [ fontSize (rem 0.8), textTransform uppercase ] ] [ text <| I18n.pointsLabel lang ]
            ]
        ]


viewTaskGrid : Language -> Participant -> FrontendModel -> Html FrontendMsg
viewTaskGrid lang kid model =
    let
        kidTasks =
            Dict.values model.tasks

                |> List.filter (\t -> t.active && (List.isEmpty t.assignedTo || List.member kid.id t.assignedTo))
    in
    div []
        [ h3 [] [ text <| I18n.missionsAndChores lang ]
        , div [ Styles.grid ] (List.map (viewTaskCard kid model) kidTasks)
        ]


viewTaskCard : Participant -> FrontendModel -> Task -> Html FrontendMsg
viewTaskCard kid model task =
    let
        available =
            canClaimTask model.zone model.now kid.id task model.transactions
    in
    button
        [ onClick (ClickCompleteTask kid.id task.id)
        , Html.Styled.Attributes.disabled (not available)
        , Styles.card
            [ textAlign left
            , borderColor
                (if available then
                    hex "E2E8F0"

                 else
                    hex "BBF7D0"
                )
            , backgroundColor
                (if available then
                    hex "FFFFFF"

                 else
                    hex "F0FDF4"
                )
            , cursor
                (if available then
                    pointer

                 else
                    default
                )
            ]
        ]
        [ div [ css [ displayFlex, justifyContent spaceBetween, marginBottom (rem 0.75) ] ]
            [ span [ Styles.pill "F1F5F9" "475569" ] [ text (Debug.toString task.recurrence) ]
            , span
                [ css
                    [ fontWeight (int 800)
                    , color
                        (if available then
                            hex "16A34A"

                         else
                            hex "64748B"
                        )
                    ]
                ]
                [ text
                    (if available then
                        "+" ++ String.fromInt task.points ++ " pts"

                     else
                        "✅ Done"
                    )
                ]
            ]
        , div [ css [ fontWeight (int 700), fontSize (rem 1.05) ] ] [ text task.title ]
        ]


viewRewardShop : Participant -> Int -> FrontendModel -> Html FrontendMsg
viewRewardShop kid points model =
    let
        activeRewards =
            Dict.values model.rewards |> List.filter .active
    in
    div []
        [ h3 [] [ text (I18n.rewardShop model.lang) ]
        , div [ Styles.grid ] (List.map (viewRewardCard kid points model) activeRewards)
        ]


viewRewardCard : Participant -> Int -> FrontendModel -> Reward -> Html FrontendMsg
viewRewardCard kid points model reward =
    let
        canAfford =
            points >= reward.cost

        pct =
            if reward.cost <= 0 then
                100

            else
                (clamp 0 100 ((toFloat points / toFloat reward.cost) * 100) ) |> Basics.round
    in
    div [ Styles.card [ displayFlex, flexDirection column, justifyContent spaceBetween, property "gap" "0.75rem" ] ]
        [ div [ Styles.row 0.75 ]
            [ span [ css [ fontSize (rem 2) ] ] [ text reward.emoji ]
            , div []
                [ div [ css [ fontWeight (int 700) ] ] [ text reward.title ]
                , div [ css [ fontSize (rem 0.85), color (hex "64748B") ] ] [ text (String.fromInt reward.cost ++ " pts") ]
                ]
            ]
        , div []
            [ div [ css [ height (px 10), backgroundColor (hex "E2E8F0"), borderRadius (px 999), overflow hidden, marginBottom (rem 0.35) ] ]
                [ div
                    [ css
                        [ height (Css.pct 100)
                        , width (Css.pct (toFloat pct))
                        , backgroundColor
                            (if canAfford then
                                hex "16A34A"

                             else
                                hex (String.dropLeft 1 kid.color)
                            )
                        ]
                    ]
                    []
                ]
            , div [ css [ fontSize (rem 0.78), color (hex "64748B") ] ]
                [ text
                    (if canAfford then
                        I18n.readyToRedeem model.lang

                     else
                        I18n.morePtsNeeded model.lang (reward.cost - points) pct
                    )
                ]
            ]
        , button
            [ onClick (ClickRedeemReward kid.id reward.id)
            , Html.Styled.Attributes.disabled (not canAfford)
            , Styles.actionButton canAfford "16A34A"
            ]
            [ text
                (if canAfford then
                    I18n.redeemNow model.lang

                 else
                    I18n.lockedLabel model.lang
                )
            ]
        ]


viewLedger : FrontendModel -> Html FrontendMsg
viewLedger model =
    div [ Styles.card [] ]
        [ h3 [ css [ marginTop zero ] ] [ text (I18n.recentActivity model.lang) ]
        , if Dict.isEmpty model.transactions then
            p [ css [ color (hex "64748B"), margin zero ] ] [ text (I18n.noActivityYet model.lang) ]

          else
            div []
                (Dict.values model.transactions
                    |> List.reverse
                    |> List.take 15
                    |> List.map
                        (\tx ->
                            let
                                kidLabel =
                                    Dict.get tx.participantId model.participants
                                        |> Maybe.map (\p -> p.avatar ++ " " ++ p.name)
                                        |> Maybe.withDefault tx.participantId
                            in
                            div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, padding2 (rem 0.5) zero, borderBottom3 (px 1) solid (hex "F1F5F9") ] ]
                                [ span [] [ text (kidLabel ++ " — " ++ tx.description) ]
                                , div [ Styles.row 0.5 ]
                                    [ span
                                        [ css
                                            [ fontWeight (int 700)
                                            , color
                                                (if tx.amount >= 0 then
                                                    hex "16A34A"

                                                 else
                                                    hex "DC2626"
                                                )
                                            ]
                                        ]
                                        [ text (String.fromInt tx.amount ++ " pts") ]
                                    , if model.isParentMode then
                                        button
                                            [ onClick (ClickDeleteTransaction tx.id)
                                            , css
                                                [ backgroundColor (hex "FEE2E2")
                                                , color (hex "991B1B")
                                                , border zero
                                                , borderRadius (px 8)
                                                , padding2 (rem 0.25) (rem 0.5)
                                                , cursor pointer
                                                , fontSize (rem 0.8)
                                                ]
                                            ]
                                            [ text (I18n.deleteButton model.lang) ]

                                      else
                                        text ""
                                    ]
                                ]
                        )
                )
        ]