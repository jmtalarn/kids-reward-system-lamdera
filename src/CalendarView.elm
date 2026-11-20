module CalendarView exposing (viewCalendarTab)

import Css exposing (..)
import Dict
import Domain exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import I18n
import Styles
import Time
import Types exposing (..)


msPerDay : Int
msPerDay =
    86400000


viewCalendarTab : Participant -> FrontendModel -> Html FrontendMsg
viewCalendarTab kid model =
    let
        weekDays =
            daysOfWeek model.zone model.selectedDate

        kidTasks =
         Dict.values model.tasks
             |> List.filter
                 (\t ->
                     isTaskActiveOnDate model.zone model.selectedDate t
                         && (List.isEmpty t.assignedTo || List.member kid.id t.assignedTo)
                 )
    in
    div []
        [ viewWeekNav kid model
        , div
            [ css
                [ property "display" "grid"
                , property "grid-template-columns" "repeat(7, 1fr)"
                , property "gap" "0.4rem"
                , marginBottom (rem 1.5)
                ]
            ]
            (List.map (viewDayCell kid model) weekDays)
        , h3 [] [ text (I18n.tasksForLabel model.lang ++ " " ++ formatDayLabel model.zone model.selectedDate) ]
        , div [ Styles.grid ] (List.map (viewCalendarTaskCard kid model) kidTasks)
        ]


viewWeekNav : Participant -> FrontendModel -> Html FrontendMsg
viewWeekNav kid model =
    let
        prevWeek =
            Time.millisToPosix (Time.posixToMillis model.selectedDate - 7 * msPerDay)

        nextWeek =
            Time.millisToPosix (Time.posixToMillis model.selectedDate + 7 * msPerDay)
    in
    div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, marginBottom (rem 1) ] ]
        [ button [ onClick (SelectCalendarDate prevWeek), Styles.actionButton True "475569" ] [ text (I18n.prevWeek model.lang) ]
        , button [ onClick (SelectCalendarDate model.now), Styles.actionButton True "2563EB" ] [ text (I18n.today model.lang) ]
        , button [ onClick (SelectCalendarDate nextWeek), Styles.actionButton True "475569" ] [ text (I18n.nextWeek model.lang) ]
        ]


viewDayCell : Participant -> FrontendModel -> Time.Posix -> Html FrontendMsg
viewDayCell kid model dayPosix =
    let
        isSelected =
            isSameDay model.zone dayPosix model.selectedDate

        isToday =
            isSameDay model.zone dayPosix model.now

        isFuture =
            not isToday && Time.posixToMillis dayPosix > Time.posixToMillis model.now

        dayPoints =
            model.transactions
                |> Dict.values
                |> List.filter (\tx -> tx.participantId == kid.id && tx.amount > 0 && isSameDay model.zone tx.timestamp dayPosix)
                |> List.map .amount
                |> List.sum
    in
    button
        [ onClick (SelectCalendarDate dayPosix)
        , Html.Styled.Attributes.disabled isFuture
        , css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , padding2 (rem 0.6) (rem 0.25)
            , borderRadius (px 12)
            , cursor
                (if isFuture then
                    notAllowed

                 else
                    pointer
                )
            , opacity
                (if isFuture then
                    num 0.45

                 else
                    num 1
                )
            , border3 (px 2)
                solid
                (if isSelected then
                    hex (String.dropLeft 1 kid.color)

                 else if isToday then
                    hex "2563EB"

                 else
                    hex "E2E8F0"
                )
            , backgroundColor
                (if isSelected then
                    hex "EFF6FF"

                 else
                    hex "FFFFFF"
                )
            ]
        ]
        [ span [ css [ fontSize (rem 0.72), color (hex "64748B"), fontWeight (int 700) ] ]
            [ text (weekdayShort model.zone dayPosix) ]
        , span [ css [ fontSize (rem 1.1), fontWeight (int 800), margin2 (rem 0.2) zero ] ]
            [ text (String.fromInt (Time.toDay model.zone dayPosix)) ]
        , span [ css [ fontSize (rem 0.7), fontWeight (int 700), color (hex "16A34A") ] ]
            [ text
                (if dayPoints > 0 then
                    "+" ++ String.fromInt dayPoints

                 else
                    "·"
                )
            ]
        ]


viewCalendarTaskCard : Participant -> FrontendModel -> Task -> Html FrontendMsg
viewCalendarTaskCard kid model task =
    let
        available =
            canClaimTask model.zone model.selectedDate kid.id task model.transactions
    in
    button
        [ onClick (ClickCompleteTaskForDate kid.id task.id model.selectedDate)
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
            ]
        ]
        [ div [ css [ displayFlex, justifyContent spaceBetween, marginBottom (rem 0.5) ] ]
            [ span [ Styles.pill "F1F5F9" "475569" ] [ text (I18n.recurrenceFormat model.lang task.recurrence) ]
            , span [ css [ fontWeight (int 800), color (if available then hex "16A34A" else hex "64748B") ] ]
                [ text (if available then "+" ++ String.fromInt task.points ++ " pts" else "✅ Done") ]
            ]
        , div [ css [ fontWeight (int 700) ] ] [ text task.title ]
        ]


daysOfWeek : Time.Zone -> Time.Posix -> List Time.Posix
daysOfWeek zone anchor =
    let
        offset =
            case Time.toWeekday zone anchor of
                Time.Mon ->
                    0

                Time.Tue ->
                    1

                Time.Wed ->
                    2

                Time.Thu ->
                    3

                Time.Fri ->
                    4

                Time.Sat ->
                    5

                Time.Sun ->
                    6

        mondayMillis =
            Time.posixToMillis anchor - (offset * msPerDay)
    in
    List.range 0 6 |> List.map (\d -> Time.millisToPosix (mondayMillis + d * msPerDay))


weekdayShort : Time.Zone -> Time.Posix -> String
weekdayShort zone t =
    case Time.toWeekday zone t of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


formatDayLabel : Time.Zone -> Time.Posix -> String
formatDayLabel zone t =
    weekdayShort zone t ++ " " ++ String.fromInt (Time.toDay zone t)
