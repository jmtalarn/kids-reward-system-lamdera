module Domain exposing (..)

import Dict exposing (Dict)
import Time


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
    }


type Recurrence
    = OneTime
    | Daily
    | Weekly
    | Monthly


type alias Task =
    { id : TaskId
    , title : String
    , description : String
    , points : Int
    , recurrence : Recurrence
    , assignedTo : List ParticipantId
    , active : Bool
    , createdAt : Time.Posix
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
balanceFor participantId transactions =
    transactions
        |> Dict.values
        |> List.filter (\tx -> tx.participantId == participantId)
        |> List.map .amount
        |> List.sum