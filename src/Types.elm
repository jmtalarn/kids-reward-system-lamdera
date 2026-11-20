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
    , lang : Language
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
    , activeTab : Tab
    , celebration : Maybe Celebration
    , celebrationCount : Int
    , selectedDate : Time.Posix
    , lang : Language
    , newTaskDays : List Time.Weekday
    , editingKidId : Maybe ParticipantId
    , editingTaskId : Maybe TaskId
    , editingRewardId : Maybe RewardId
    , showPinPad: Bool
    , pinLength : Int
    , emojiPicker : Maybe EmojiTarget
    , newTaskRequiresApproval : Bool
    , newTaskStartDate : String
    , newTaskEndDate : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotZone Time.Zone
    | Tick Time.Posix
    | SelectParticipant (Maybe ParticipantId)
    | ClickCompleteTask ParticipantId TaskId
    | ClickRedeemReward ParticipantId RewardId
    | DismissError
    | NoOp
      -- Parent PIN & Admin Forms
    | OpenPinPad
    | ClosePinPad
    | PressPinDigit String
    | BackspacePin
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
    | SelectTab Tab
    | ClearCelebration Int
    | SelectCalendarDate Time.Posix
    | ClickCompleteTaskForDate ParticipantId TaskId Time.Posix
    | ClickDeleteTransaction TransactionId
    | SetLanguage Language
    | ToggleTaskDay Time.Weekday
    | SetTaskDays (List Time.Weekday)
    | StartEditKid Participant
    | CancelEditKid
    | StartEditTask Task
    | CancelEditTask
    | StartEditReward Reward
    | CancelEditReward
    | ClickToggleGoal ParticipantId RewardId
    | OpenEmojiPicker EmojiTarget
    | CloseEmojiPicker
    | PickEmoji String
    | ToggleNewTaskRequiresApproval
    | ClickApproveTransaction TransactionId
    | ClickRejectTransaction TransactionId
    | UpdateNewTaskStartDate String 
    | UpdateNewTaskEndDate String


type ToBackend
    = RequestInitialState
    | VerifyParentPin String
    | LogoutParent
    | CreateParticipant { name : String, avatar : String, color : String }
    | CreateTask { title : String, description : String, points : Int, recurrence : Recurrence, assignedTo : List ParticipantId, requiresApproval : Bool, startDate : Maybe Time.Posix, endDate : Maybe Time.Posix }
    | CreateReward { title : String, description : String, cost : Int, emoji : String }
    | AddManualAdjustment { participantId : ParticipantId, amount : Int, reason : String }
    | ToggleTaskActive TaskId
    | ToggleRewardActive RewardId
    | CompleteTask { participantId : ParticipantId, taskId : TaskId, forDate : Time.Posix }
    | RedeemReward { participantId : ParticipantId, rewardId : RewardId }
    | DeleteTransaction TransactionId
    | UpdateParticipant { id : ParticipantId, name : String, avatar : String, color : String }
    | UpdateTask { id : TaskId, title : String, description : String, points : Int, recurrence : Recurrence, assignedTo : List ParticipantId, requiresApproval : Bool, startDate : Maybe Time.Posix, endDate : Maybe Time.Posix }
    | UpdateReward { id : RewardId, title : String, description : String, cost : Int, emoji : String }
    | ToggleParticipantGoal { participantId : ParticipantId, rewardId : RewardId }
    | ApproveTransaction TransactionId
    | RejectTransaction TransactionId
    | UpdateLanguage Language


type BackendMsg
    = GotTimeForToBackend Lamdera.ClientId ToBackend Time.Posix


type ToFrontend
    = StateSynced
        { participants : Dict ParticipantId Participant
        , tasks : Dict TaskId Task
        , rewards : Dict RewardId Reward
        , transactions : Dict TransactionId PointTransaction
        , pinLength : Int
        , lang : Language
        }
    | ParentPinVerified Bool
    | ActionFailed String



type Tab
    = TasksTab
    | ShopTab
    | ActivityTab
    | AdminTab
    | CalendarTab


type alias Celebration =
    { id : Int
    , title : String
    , subtitle : String
    , isReward : Bool
    }


type Language
    = En
    | Es
    | Ca

type EmojiTarget
    = KidEmojiTarget
    | RewardEmojiTarget