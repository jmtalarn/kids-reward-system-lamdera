module Types exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Domain exposing (..)
import Lamdera
import Time
import Url


type alias BackendModel =
    { nextId : Int
    , participants : Dict ParticipantId Participant
    , tasks : Dict TaskId Task
    , rewards : Dict RewardId Reward
    , transactions : Dict TransactionId PointTransaction
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , participants : Dict ParticipantId Participant
    , tasks : Dict TaskId Task
    , rewards : Dict RewardId Reward
    , transactions : Dict TransactionId PointTransaction
    , selectedParticipant : Maybe ParticipantId
    , errorMessage : Maybe String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SelectParticipant (Maybe ParticipantId)
    | ClickCompleteTask ParticipantId TaskId
    | ClickRedeemReward ParticipantId RewardId
    | DismissError


type ToBackend
    = RequestInitialState
    | CreateParticipant { name : String, avatar : String, color : String }
    | CreateTask { title : String, description : String, points : Int, recurrence : Recurrence, assignedTo : List ParticipantId }
    | CreateReward { title : String, description : String, cost : Int, emoji : String }
    | CompleteTask { participantId : ParticipantId, taskId : TaskId }
    | RedeemReward { participantId : ParticipantId, rewardId : RewardId }


type BackendMsg
    = GotTimeForToBackend Lamdera.ClientId ToBackend Time.Posix


type ToFrontend
    = StateSynced
        { participants : Dict ParticipantId Participant
        , tasks : Dict TaskId Task
        , rewards : Dict RewardId Reward
        , transactions : Dict TransactionId PointTransaction
        }
    | ActionFailed String
