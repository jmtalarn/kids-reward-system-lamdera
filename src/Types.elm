module Types exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Domain exposing (..)
import Lamdera
import Set exposing (Set)
import Time
import Url


type alias BackendModel =
    { nextId : Int
    , authenticatedSessions : Set Lamdera.SessionId
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
    , now : Time.Posix
    , zone : Time.Zone
    -- Parent Mode & Forms
    , isParentMode : Bool
    , pinInput : String
    , newKidName : String
    , newKidAvatar : String
    , newTaskTitle : String
    , newTaskPoints : String
    , newTaskRecurrence : Recurrence
    , newRewardTitle : String
    , newRewardCost : String
    , newRewardEmoji : String
    , bonusAmount : String
    , bonusReason : String
    , newKidColor : String
    , newTaskAssignedTo : List ParticipantId
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SelectParticipant (Maybe ParticipantId)
    | ClickCompleteTask ParticipantId TaskId
    | ClickRedeemReward ParticipantId RewardId
    | DismissError
      -- Parent PIN & Admin Forms
    | UpdatePinInput String
    | SubmitPin
    | LockParentMode
    | UpdateNewKidName String
    | UpdateNewKidAvatar String
    | SubmitNewKid
    | UpdateNewTaskTitle String
    | UpdateNewTaskPoints String
    | UpdateNewTaskRecurrence Recurrence
    | SubmitNewTask
    | UpdateNewRewardTitle String
    | UpdateNewRewardCost String
    | UpdateNewRewardEmoji String
    | SubmitNewReward
    | UpdateBonusAmount String
    | UpdateBonusReason String
    | SubmitBonus ParticipantId
    | SelectNewKidColor String
    | ToggleTaskAssignee ParticipantId
    | ClickToggleTaskActive TaskId
    | ClickToggleRewardActive RewardId
    | GotZone Time.Zone
    | Tick Time.Posix


type ToBackend
    = RequestInitialState
    | VerifyParentPin String
    | LogoutParent
    | CreateParticipant { name : String, avatar : String, color : String }
    | CreateTask { title : String, description : String, points : Int, recurrence : Recurrence, assignedTo : List ParticipantId }
    | CreateReward { title : String, description : String, cost : Int, emoji : String }
    | AddManualAdjustment { participantId : ParticipantId, amount : Int, reason : String }
    | ToggleTaskActive TaskId
    | ToggleRewardActive RewardId
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
    | ParentPinVerified Bool
    | ActionFailed String
