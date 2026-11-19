module AdminView exposing (viewAdminPanel)

import Css exposing (..)
import Dict
import Domain exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (id, css, placeholder, selected, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import I18n
import Styles
import Time
import Types exposing (..)


kidColors : List String
kidColors =
    [ "#F59E0B", "#3B82F6", "#10B981", "#EC4899", "#8B5CF6", "#EF4444" ]


viewAdminPanel : FrontendModel -> Html FrontendMsg
viewAdminPanel model =
    div [ css [ displayFlex, flexDirection column, property "gap" "1.25rem" ] ]
        [ viewLanguageCard model.lang
        , viewKidForm model
        , viewTaskForm model
        , viewRewardForm model
        , viewBonusForm model
        , viewManageItems model
        ]


sectionHeader : String -> String -> Html msg
sectionHeader icon title =
    h3 [ css [ marginTop zero, marginBottom (rem 0.85), fontSize (rem 1.05), color (hex "1E293B"), displayFlex, alignItems center, property "gap" "0.45rem" ] ]
        [ span [] [ text icon ]
        , span [] [ text title ]
        ]


viewLanguageCard : Language -> Html FrontendMsg
viewLanguageCard current =
    let
        langs =
            [ ( Ca, "Català" )
            , ( Es, "Español" )
            , ( En, "English" )
            ]
    in
    div [ Styles.card [ displayFlex, justifyContent spaceBetween, alignItems center, flexWrap wrap, property "gap" "0.75rem" ] ]
        [ div []
            [ div [ css [ fontWeight (int 700), fontSize (rem 1), color (hex "0F172A") ] ] [ text (I18n.languageLabel current) ]
            , div [ css [ fontSize (rem 0.82), color (hex "64748B") ] ] [ text (I18n.chooseLanguage current) ]
            ]
        , div [ Styles.row 0.4 ]
            (List.map
                (\( l, labelText ) ->
                    let
                        isSelected =
                            current == l
                    in
                    button
                        [ type_ "button"
                        , onClick (SetLanguage l)
                        , css
                            [ padding2 (rem 0.45) (rem 0.85)
                            , borderRadius (px 10)
                            , fontWeight (int 700)
                            , fontSize (rem 0.85)
                            , cursor pointer
                            , border3 (px 2) solid (if isSelected then hex "2563EB" else hex "E2E8F0")
                            , backgroundColor (if isSelected then hex "EFF6FF" else hex "FFFFFF")
                            , color (if isSelected then hex "2563EB" else hex "475569")
                            ]
                        ]
                        [ text labelText ]
                )
                langs
            )
        ]

viewKidForm : FrontendModel -> Html FrontendMsg
viewKidForm model =
    let
        isEditing =
            model.editingKidId /= Nothing
    in
    div [ Styles.card [] ]
        [ sectionHeader
            (if isEditing then
                "✏️"

             else
                "🧒"
            )
            (if isEditing then
                "Edit Kid"

             else
                "Add Kid"
            )
        , form [ onSubmit SubmitNewKid, Styles.row 0.6 ]
            [ input [ placeholder (I18n.avatarPlaceholder model.lang), value model.newKidAvatar, onInput UpdateNewKidAvatar, Styles.field [ width (px 58), textAlign center ] ] []
            , input [ id "edit-kid-input", placeholder (I18n.kidNamePlaceholder model.lang), value model.newKidName, onInput UpdateNewKidName, Styles.field [ flexGrow (num 1) ] ] []
            , div [ Styles.row 0.35 ] (List.map (viewColorSwatch model.newKidColor) kidColors)
            , button [ type_ "submit", Styles.actionButton True "2563EB" ]
                [ text
                    (if isEditing then
                        I18n.saveButton model.lang

                     else
                        I18n.addButton model.lang
                    )
                ]
            , if isEditing then
                button [ type_ "button", onClick CancelEditKid, Styles.actionButton True "64748B" ] [ text (I18n.cancelButton model.lang) ]

              else
                text ""
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
            [ width (px 28)
            , height (px 28)
            , borderRadius (pct 50)
            , backgroundColor (hex (String.dropLeft 1 colorHex))
            , cursor pointer
            , border3 (px 3) solid (if isSelected then hex "0F172A" else hex "E2E8F0")
            ]
        ]
        []


viewTaskForm : FrontendModel -> Html FrontendMsg
viewTaskForm model =
    let
        isSpecificDays =
            case model.newTaskRecurrence of
                SpecificDays _ ->
                    True

                _ ->
                    False
        isEditing = model.editingTaskId /= Nothing
    in
    div [ Styles.card [] ]
        [ sectionHeader
            (if isEditing then
                "✏️"

             else
                "🎯"
            )
            (if isEditing then
                I18n.editTaskTitle model.lang

             else
                I18n.createTaskTitle model.lang
            )
        , form [ onSubmit SubmitNewTask, Styles.row 0.6 ]
            [ input [ id "edit-task-input", placeholder (I18n.taskTitlePlaceholder model.lang), value model.newTaskTitle, onInput UpdateNewTaskTitle, Styles.field [ flexGrow (num 1) ] ] []
            , input [ type_ "number", placeholder (I18n.ptsPlaceholder model.lang), value model.newTaskPoints, onInput UpdateNewTaskPoints, Styles.field [ width (px 75) ] ] []
            , select [ onInput (toRecurrence >> UpdateNewTaskRecurrence), Styles.field [] ]
                [ option [ value "Daily", selected (model.newTaskRecurrence == Daily) ] [ text (I18n.dailyLabel model.lang) ]
                , option [ value "SpecificDays", selected isSpecificDays ] [ text (I18n.specificDays model.lang) ]
                , option [ value "Weekly", selected (model.newTaskRecurrence == Weekly) ] [ text (I18n.weeklyLabel model.lang) ]
                , option [ value "Monthly", selected (model.newTaskRecurrence == Monthly) ] [ text (I18n.monthlyLabel model.lang) ]
                , option [ value "OneTime", selected (model.newTaskRecurrence == OneTime) ] [ text (I18n.oneTime model.lang) ]
                ]
            , button [ type_ "submit", Styles.actionButton True "2563EB" ]
                [ text
                    (if isEditing then
                        I18n.saveButton model.lang

                     else
                        I18n.addButton model.lang
                    )
                ]
            , if isEditing then
                button [ type_ "button", onClick CancelEditTask, Styles.actionButton True "64748B" ] [ text (I18n.cancelButton model.lang) ]

              else
                text ""
            ]
        , if isSpecificDays then
            viewDayPicker model

          else
            text ""
        , div [ Styles.row 0.4, css [ marginTop (rem 0.75) ] ]
            (span [ css [ fontSize (rem 0.85), fontWeight (int 600), color (hex "64748B") ] ]
                [ text
                    (if List.isEmpty model.newTaskAssignedTo then
                        I18n.assignToEveryone model.lang

                     else
                        I18n.assignTo model.lang
                    )
                ]
                :: (Dict.values model.participants |> List.map (viewAssigneeChip model.newTaskAssignedTo))
            )
        ]


viewDayPicker : FrontendModel -> Html FrontendMsg
viewDayPicker model =
    let
        allDays =
            [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri, Time.Sat, Time.Sun ]
    in
    div [ css [ marginTop (rem 0.75), padding (rem 0.75), backgroundColor (hex "F8FAFC"), borderRadius (px 12), border3 (px 1) solid (hex "E2E8F0") ] ]
        [ div [ Styles.row 0.35, css [ marginBottom (rem 0.5) ] ]
            (List.map (viewDayChip model.lang model.newTaskDays) allDays)
        , div [ Styles.row 0.4 ]
            [ button [ type_ "button", onClick (SetTaskDays [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri ]), Styles.pill "E2E8F0" "334155", css [ border zero, cursor pointer ] ] [ text (I18n.monFri model.lang) ]
            , button [ type_ "button", onClick (SetTaskDays [ Time.Sat, Time.Sun ]), Styles.pill "E2E8F0" "334155", css [ border zero, cursor pointer ] ] [ text (I18n.satSun model.lang) ]
            ]
        ]


viewDayChip : Language -> List Time.Weekday -> Time.Weekday -> Html FrontendMsg
viewDayChip lang selectedDays day =
    let
        isSelected =
            List.member day selectedDays
    in
    button
        [ type_ "button"
        , onClick (ToggleTaskDay day)
        , css
            [ padding2 (rem 0.35) (rem 0.65)
            , borderRadius (px 999)
            , fontSize (rem 0.8)
            , fontWeight (int 700)
            , cursor pointer
            , border3 (px 2) solid (if isSelected then hex "2563EB" else hex "CBD5E1")
            , backgroundColor (if isSelected then hex "2563EB" else hex "FFFFFF")
            , color (if isSelected then hex "FFFFFF" else hex "475569")
            ]
        ]
        [ text (I18n.shortDayName lang day) ]


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
            [ padding2 (rem 0.3) (rem 0.7)
            , borderRadius (px 999)
            , fontSize (rem 0.8)
            , fontWeight (int 700)
            , cursor pointer
            , border3 (px 2) solid (if isSelected then hex (String.dropLeft 1 kid.color) else hex "CBD5E1")
            , backgroundColor (if isSelected then hex "EFF6FF" else hex "FFFFFF")
            ]
        ]
        [ text (kid.avatar ++ " " ++ kid.name) ]


viewRewardForm : FrontendModel -> Html FrontendMsg
viewRewardForm model =
    let
        isEditing =
            model.editingRewardId /= Nothing
    in
    div [ Styles.card [] ]
        [ sectionHeader
            (if isEditing then
                "✏️"

             else
                "🎁"
            )
            (if isEditing then
                I18n.editRewardTitle model.lang

             else
                I18n.createRewardTitle model.lang
            )
        , form [ onSubmit SubmitNewReward, Styles.row 0.6 ]
            [ input [ placeholder (I18n.emojiPlaceholder model.lang), value model.newRewardEmoji, onInput UpdateNewRewardEmoji, Styles.field [ width (px 58), textAlign center ] ] []
            , input [ id "edit-reward-input", placeholder (I18n.rewardTitlePlaceholder model.lang), value model.newRewardTitle, onInput UpdateNewRewardTitle, Styles.field [ flexGrow (num 1) ] ] []
            , input [ type_ "number", placeholder (I18n.costPlaceholder model.lang), value model.newRewardCost, onInput UpdateNewRewardCost, Styles.field [ width (px 75) ] ] []
            , button [ type_ "submit", Styles.actionButton True "2563EB" ]
                [ text
                    (if isEditing then
                        I18n.saveButton model.lang

                     else
                        I18n.addButton model.lang
                    )
                ]
            , if isEditing then
                button [ type_ "button", onClick CancelEditReward, Styles.actionButton True "64748B" ] [ text (I18n.cancelButton model.lang) ]

              else
                text ""
            ]
        ]



viewBonusForm : FrontendModel -> Html FrontendMsg
viewBonusForm model =
    case model.selectedParticipant |> Maybe.andThen (\pid -> Dict.get pid model.participants) of
        Just kid ->
            div [ Styles.card [] ]
                [ sectionHeader "⚡" (I18n.quickAdjustmentTitle model.lang kid.avatar kid.name)
                , form [ onSubmit (SubmitBonus kid.id), Styles.row 0.6 ]
                    [ input [ type_ "number", placeholder (I18n.bonusAmountPlaceholder model.lang), value model.bonusAmount, onInput UpdateBonusAmount, Styles.field [ width (px 85) ] ] []
                    , input [ placeholder (I18n.bonusReasonPlaceholder model.lang), value model.bonusReason, onInput UpdateBonusReason, Styles.field [ flexGrow (num 1) ] ] []
                    , button [ type_ "submit", Styles.actionButton True "16A34A" ] [ text (I18n.applyButton model.lang) ]
                    ]
                ]

        Nothing ->
            text ""


viewManageItems : FrontendModel -> Html FrontendMsg
viewManageItems model =
    div [ Styles.card [] ]
        [ sectionHeader "📦" (I18n.manageSectionTitle model.lang)
        , div [ css [ displayFlex, flexDirection column, property "gap" "0.5rem", marginBottom (rem 1.25) ] ]
            (Dict.values model.participants
                |> List.map
                    (\kid ->
                        div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, padding (rem 0.6), borderRadius (px 10), backgroundColor (hex "F8FAFC") ] ]
                            [ span [ css [ fontWeight (int 600), color (hex (String.dropLeft 1 kid.color)) ] ]
                                [ text (kid.avatar ++ " " ++ kid.name) ]
                            , button [ type_ "button", onClick (StartEditKid kid), Styles.actionButton True "2563EB" ] [ text (I18n.editButton model.lang) ]
                            ]
                    )
            )
        , div [ css [ displayFlex, flexDirection column, property "gap" "0.5rem", marginBottom (rem 1.25) ] ]
            (Dict.values model.tasks
                |> List.map
                    (\t ->
                        div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, padding (rem 0.6), borderRadius (px 10), backgroundColor (hex "F8FAFC") ] ]
                            [ span [ css [ fontWeight (int 600), opacity (if t.active then num 1 else num 0.5) ] ]
                                [ text (t.title ++ " (+" ++ String.fromInt t.points ++ " pts)") ]
                            , div [ Styles.row 0.4 ]
                                [ button [ type_ "button", onClick (StartEditTask t), Styles.actionButton True "2563EB" ] [ text (I18n.editButton model.lang) ]
                                , button
                                    [ type_ "button"
                                    , onClick (ClickToggleTaskActive t.id)
                                    , Styles.actionButton True (if t.active then "64748B" else "16A34A")
                                    ]
                                    [ text (if t.active then I18n.archiveButtonText model.lang else I18n.restoreButtonText model.lang) ]
                                ]
                            ]
                    )
            )
        , div [ css [ displayFlex, flexDirection column, property "gap" "0.5rem" ] ]
            (Dict.values model.rewards
                |> List.map
                    (\r ->
                        div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, padding (rem 0.6), borderRadius (px 10), backgroundColor (hex "F8FAFC") ] ]
                            [ span [ css [ fontWeight (int 600), opacity (if r.active then num 1 else num 0.5) ] ]
                                [ text (r.emoji ++ " " ++ r.title ++ " (" ++ String.fromInt r.cost ++ " pts)") ]
                            , div [ Styles.row 0.4 ]
                                [ button [ type_ "button", onClick (StartEditReward r), Styles.actionButton True "2563EB" ] [ text (I18n.editButton model.lang) ]
                                , button
                                    [ type_ "button"
                                    , onClick (ClickToggleRewardActive r.id)
                                    , Styles.actionButton True (if r.active then "64748B" else "16A34A")
                                    ]
                                    [ text (if r.active then I18n.archiveButtonText model.lang else I18n.restoreButtonText model.lang) ]
                                ]
                            ]
                    )
            )
        ]

toRecurrence : String -> Recurrence
toRecurrence str =
    case str of
        "SpecificDays" ->
            SpecificDays []

        "OneTime" ->
            OneTime

        "Weekly" ->
            Weekly

        "Monthly" ->
            Monthly

        _ ->
            Daily
