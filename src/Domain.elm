module Domain exposing (..)

import Dict exposing (Dict)
import Time
import Env

type alias ParticipantId =
    String


type alias TaskId =
    String


type alias RewardId =
    String


type alias TransactionId =
    String


type alias Participant =
    { id : ParticipantId
    , name : String
    , avatar : String
    , color : String
    , createdAt : Time.Posix
    , goalRewardIds : List RewardId
    }


type Recurrence
    = OneTime
    | Daily
    | Weekly
    | Monthly
    | SpecificDays (List Time.Weekday)


type alias Task =
    { id : TaskId
    , title : String
    , description : String
    , points : Int
    , recurrence : Recurrence
    , assignedTo : List ParticipantId
    , active : Bool
    , createdAt : Time.Posix
    , requiresApproval : Bool
    }


type alias Reward =
    { id : RewardId
    , title : String
    , description : String
    , cost : Int
    , emoji : String
    , active : Bool
    , createdAt : Time.Posix
    }


type TransactionType
    = EarnedFromTask TaskId
    | ManualAdjustment String
    | SpentOnReward RewardId


type alias PointTransaction =
    { id : TransactionId
    , participantId : ParticipantId
    , amount : Int
    , type_ : TransactionType
    , description : String
    , timestamp : Time.Posix
    , approvedBy : Maybe String
    }


-- Derived helper usable on both Frontend and Backend
balanceFor : ParticipantId -> Dict TransactionId PointTransaction -> Int
balanceFor pid transactions =
    transactions
        |> Dict.values
        |> List.filter
            (\tx ->
                tx.participantId == pid && (tx.amount < 0 || tx.approvedBy /= Nothing)
            )
        |> List.map .amount
        |> List.sum



canClaimTask : Time.Zone -> Time.Posix -> ParticipantId -> Task -> Dict TransactionId PointTransaction -> Bool
canClaimTask zone now participantId task transactions =
    let
        pastClaims =
            transactions
                |> Dict.values
                |> List.filter
                    (\tx ->
                        tx.participantId == participantId && tx.type_ == EarnedFromTask task.id
                    )
    in
    case task.recurrence of
        OneTime ->
            List.isEmpty pastClaims

        Daily ->
            not (List.any (\tx -> isSameDay zone tx.timestamp now) pastClaims)

        Weekly ->
            not (List.any (\tx -> isSameWeek zone tx.timestamp now) pastClaims)

        Monthly ->
            not (List.any (\tx -> isSameMonth zone tx.timestamp now) pastClaims)
        
        SpecificDays allowedDays ->
            List.member (Time.toWeekday zone now) allowedDays
                && not (List.any (\tx -> isSameDay zone tx.timestamp now) pastClaims)  


isSameMonth : Time.Zone -> Time.Posix -> Time.Posix -> Bool
isSameMonth zone t1 t2 =
    (Time.toYear zone t1 == Time.toYear zone t2)
        && (Time.toMonth zone t1 == Time.toMonth zone t2)


isSameDay : Time.Zone -> Time.Posix -> Time.Posix -> Bool
isSameDay zone t1 t2 =
    isSameMonth zone t1 t2
        && (Time.toDay zone t1 == Time.toDay zone t2)


isSameWeek : Time.Zone -> Time.Posix -> Time.Posix -> Bool
isSameWeek zone t1 t2 =
    startOfWeekMillis zone t1 == startOfWeekMillis zone t2


startOfWeekMillis : Time.Zone -> Time.Posix -> Int
startOfWeekMillis zone time =
    let
        dayOffset =
            case Time.toWeekday zone time of
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

        msPerDay =
            86400000

        -- Shift by zone offset so day boundaries match local midnight
        offsetMillis =
            Env.timezoneOffsetMinutes * 60 * 1000

        dayIndex =
            (Time.posixToMillis time + offsetMillis) // msPerDay
    in
    dayIndex - dayOffset


isScheduledDay : Time.Zone -> Time.Posix -> Recurrence -> Bool
isScheduledDay zone now recurrence =
    case recurrence of
        SpecificDays allowedDays ->
            List.member (Time.toWeekday zone now) allowedDays

        _ ->
            True