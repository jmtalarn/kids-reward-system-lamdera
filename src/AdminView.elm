module AdminView exposing (viewAdminPanel)

import Css exposing (..)
import Dict
import Domain exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, selected, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Styles
import Types exposing (..)


kidColors : List String
kidColors =
    [ "#F59E0B"
    , "#3B82F6"
    , "#10B981"
    , "#EC4899"
    , "#8B5CF6"
    , "#EF4444"
    ]


viewAdminPanel : FrontendModel -> Html FrontendMsg
viewAdminPanel model =
    div [ Styles.card [ backgroundColor (hex "F8FAFC"), marginBottom (rem 1.5) ] ]
        [ h2 [ css [ marginTop zero ] ] [ text "🛠️ Parent Admin Panel" ]
        , viewKidForm model
        , viewTaskForm model
        , viewRewardForm model
        , viewBonusForm model
        , viewManageItems model
        ]


viewKidForm : FrontendModel -> Html FrontendMsg
viewKidForm model =
    div [ css [ marginBottom (rem 1.25) ] ]
        [ h3 [] [ text "Add Kid" ]
        , form [ onSubmit SubmitNewKid, Styles.row 0.5 ]
            [ input [ placeholder "Avatar", value model.newKidAvatar, onInput UpdateNewKidAvatar, css [ width (px 50) ] ] []
            , input [ placeholder "Kid name", value model.newKidName, onInput UpdateNewKidName ] []
            , div [ Styles.row 0.35 ] (List.map (viewColorSwatch model.newKidColor) kidColors)
            , button [ type_ "submit", Styles.actionButton True "2563EB" ] [ text "Add Kid" ]
            ]
        ]


viewColorSwatch : String -> String -> Html FrontendMsg
viewColorSwatch selectedColor colorHex =
    let
        isSelected =
            selectedColor == colorHex
    in
    button
        [ type_ "button"
        , onClick (SelectNewKidColor colorHex)
        , css
            [ width (px 26)
            , height (px 26)
            , borderRadius (pct 50)
            , backgroundColor (hex (String.dropLeft 1 colorHex))
            , cursor pointer
            , border3 (px 3)
                solid
                (if isSelected then
                    hex "0F172A"

                 else
                    hex "FFFFFF"
                )
            ]
        ]
        []


viewTaskForm : FrontendModel -> Html FrontendMsg
viewTaskForm model =
    div [ css [ marginBottom (rem 1.25) ] ]
        [ h3 [] [ text "Create Task" ]
        , form [ onSubmit SubmitNewTask, Styles.row 0.5 ]
            [ input [ placeholder "Task title", value model.newTaskTitle, onInput UpdateNewTaskTitle ] []
            , input [ type_ "number", placeholder "Pts", value model.newTaskPoints, onInput UpdateNewTaskPoints, css [ width (px 65) ] ] []
            , select [ onInput (toRecurrence >> UpdateNewTaskRecurrence) ]
                [ option [ value "Daily", selected (model.newTaskRecurrence == Daily) ] [ text "Daily" ]
                , option [ value "Weekly", selected (model.newTaskRecurrence == Weekly) ] [ text "Weekly" ]
                , option [ value "Monthly", selected (model.newTaskRecurrence == Monthly) ] [ text "Monthly" ]
                , option [ value "OneTime", selected (model.newTaskRecurrence == OneTime) ] [ text "OneTime" ]
                ]
            , button [ type_ "submit", Styles.actionButton True "2563EB" ] [ text "Add Task" ]
            ]
        , div [ Styles.row 0.4, css [ marginTop (rem 0.5) ] ]
            (span [ css [ fontSize (rem 0.85), color (hex "475569") ] ]
                [ text
                    (if List.isEmpty model.newTaskAssignedTo then
                        "Assign to: Everyone"

                     else
                        "Assign to:"
                    )
                ]
                :: (Dict.values model.participants
                        |> List.map (viewAssigneeChip model.newTaskAssignedTo)
                   )
            )
        ]


viewAssigneeChip : List ParticipantId -> Participant -> Html FrontendMsg
viewAssigneeChip assignedList kid =
    let
        isSelected =
            List.member kid.id assignedList
    in
    button
        [ type_ "button"
        , onClick (ToggleTaskAssignee kid.id)
        , css
            [ padding2 (rem 0.25) (rem 0.65)
            , borderRadius (px 999)
            , fontSize (rem 0.8)
            , fontWeight (int 700)
            , cursor pointer
            , border3 (px 2)
                solid
                (if isSelected then
                    hex (String.dropLeft 1 kid.color)

                 else
                    hex "CBD5E1"
                )
            , backgroundColor
                (if isSelected then
                    hex "EFF6FF"

                 else
                    hex "FFFFFF"
                )
            ]
        ]
        [ text (kid.avatar ++ " " ++ kid.name) ]


viewRewardForm : FrontendModel -> Html FrontendMsg
viewRewardForm model =
    div [ css [ marginBottom (rem 1.25) ] ]
        [ h3 [] [ text "Create Shop Reward" ]
        , form [ onSubmit SubmitNewReward, Styles.row 0.5 ]
            [ input [ placeholder "Emoji", value model.newRewardEmoji, onInput UpdateNewRewardEmoji, css [ width (px 50) ] ] []
            , input [ placeholder "Reward title", value model.newRewardTitle, onInput UpdateNewRewardTitle ] []
            , input [ type_ "number", placeholder "Cost", value model.newRewardCost, onInput UpdateNewRewardCost, css [ width (px 65) ] ] []
            , button [ type_ "submit", Styles.actionButton True "2563EB" ] [ text "Add Reward" ]
            ]
        ]


viewBonusForm : FrontendModel -> Html FrontendMsg
viewBonusForm model =
    case model.selectedParticipant of
        Just pid ->
            div []
                [ h3 [] [ text "Manual Bonus / Penalty" ]
                , form [ onSubmit (SubmitBonus pid), Styles.row 0.5 ]
                    [ input [ type_ "number", placeholder "+10 / -5", value model.bonusAmount, onInput UpdateBonusAmount, css [ width (px 75) ] ] []
                    , input [ placeholder "Reason", value model.bonusReason, onInput UpdateBonusReason ] []
                    , button [ type_ "submit", Styles.actionButton True "16A34A" ] [ text "Apply" ]
                    ]
                ]

        Nothing ->
            text ""


toRecurrence : String -> Recurrence
toRecurrence str =
    case str of
        "OneTime" ->
            OneTime

        "Weekly" ->
            Weekly

        "Monthly" ->
            Monthly

        _ ->
            Daily


viewManageItems : FrontendModel -> Html FrontendMsg
viewManageItems model =
    div [ css [ marginTop (rem 1.25), paddingTop (rem 1), borderTop3 (px 1) solid (hex "CBD5E1") ] ]
        [ h3 [] [ text "Manage Tasks" ]
        , div []
            (Dict.values model.tasks
                |> List.map
                    (\t ->
                        div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, padding2 (rem 0.35) zero ] ]
                            [ span [ css [ opacity (if t.active then num 1 else num 0.5) ] ]
                                [ text (t.title ++ " (+" ++ String.fromInt t.points ++ " pts)") ]
                            , button
                                [ type_ "button"
                                , onClick (ClickToggleTaskActive t.id)
                                , Styles.actionButton True (if t.active then "64748B" else "16A34A")
                                ]
                                [ text (if t.active then "Archive" else "Restore") ]
                            ]
                    )
            )
        , h3 [] [ text "Manage Shop Rewards" ]
        , div []
            (Dict.values model.rewards
                |> List.map
                    (\r ->
                        div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, padding2 (rem 0.35) zero ] ]
                            [ span [ css [ opacity (if r.active then num 1 else num 0.5) ] ]
                                [ text (r.emoji ++ " " ++ r.title ++ " (" ++ String.fromInt r.cost ++ " pts)") ]
                            , button
                                [ type_ "button"
                                , onClick (ClickToggleRewardActive r.id)
                                , Styles.actionButton True (if r.active then "64748B" else "16A34A")
                                ]
                                [ text (if r.active then "Archive" else "Restore") ]
                            ]
                    )
            )
        ]
