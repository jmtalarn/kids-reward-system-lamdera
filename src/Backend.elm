module Backend exposing (..)

import Dict exposing (Dict)
import Domain exposing (..)
import Lamdera exposing (ClientId, SessionId)
import Task
import Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    let
        epoch =
            Time.millisToPosix 0

        starterParticipant =
            { id = "1"
            , name = "Leo"
            , avatar = "🦁"
            , color = "#F59E0B"
            , createdAt = epoch
            }

        starterTask =
            { id = "2"
            , title = "Make your bed"
            , description = "Tidy pillows and blanket before school"
            , points = 10
            , recurrence = Daily
            , assignedTo = []
            , active = True
            , createdAt = epoch
            }

        starterOneTimeTask =
            { id = "3"
            , title = "Clean out toy box"
            , description = "Donate old toys"
            , points = 50
            , recurrence = OneTime
            , assignedTo = []
            , active = True
            , createdAt = epoch
            }

        starterReward =
            { id = "4"
            , title = "30 mins Extra Screen Time"
            , description = "Valid on weekends"
            , cost = 30
            , emoji = "🎮"
            , active = True
            , createdAt = epoch
            }
    in
    ( { nextId = 5
      , participants = Dict.singleton "1" starterParticipant
      , tasks =
            Dict.fromList
                [ ( "2", starterTask )
                , ( "3", starterOneTimeTask )
                ]
      , rewards = Dict.singleton "4" starterReward
      , transactions = Dict.empty
      }
    , Cmd.none
    )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        RequestInitialState ->
            ( model
            , Lamdera.sendToFrontend clientId (snapshotFor model)
            )

        _ ->
            -- Grab current server timestamp before mutating state
            ( model
            , Task.perform (GotTimeForToBackend clientId msg) Time.now
            )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        GotTimeForToBackend clientId toBackendMsg now ->
            handleTimedAction clientId toBackendMsg now model


handleTimedAction : ClientId -> ToBackend -> Time.Posix -> BackendModel -> ( BackendModel, Cmd BackendMsg )
handleTimedAction clientId msg now model =
    let
        newId =
            String.fromInt model.nextId
    in
    case msg of
        RequestInitialState ->
            ( model, Lamdera.sendToFrontend clientId (snapshotFor model) )

        CreateParticipant data ->
            let
                participant =
                    { id = newId
                    , name = data.name
                    , avatar = data.avatar
                    , color = data.color
                    , createdAt = now
                    }

                updatedModel =
                    { model
                        | nextId = model.nextId + 1
                        , participants = Dict.insert newId participant model.participants
                    }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

        CreateTask data ->
            let
                task =
                    { id = newId
                    , title = data.title
                    , description = data.description
                    , points = data.points
                    , recurrence = data.recurrence
                    , assignedTo = data.assignedTo
                    , active = True
                    , createdAt = now
                    }

                updatedModel =
                    { model
                        | nextId = model.nextId + 1
                        , tasks = Dict.insert newId task model.tasks
                    }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

        CreateReward data ->
            let
                reward =
                    { id = newId
                    , title = data.title
                    , description = data.description
                    , cost = data.cost
                    , emoji = data.emoji
                    , active = True
                    , createdAt = now
                    }

                updatedModel =
                    { model
                        | nextId = model.nextId + 1
                        , rewards = Dict.insert newId reward model.rewards
                    }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

        CompleteTask { participantId, taskId } ->
            case ( Dict.get participantId model.participants, Dict.get taskId model.tasks ) of
                ( Just _, Just task ) ->
                    if not task.active then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed "This task is inactive.") )

                    else if task.recurrence == OneTime && alreadyClaimedOneTime participantId taskId model.transactions then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed "One-time task already completed!") )

                    else
                        let
                            tx =
                                { id = newId
                                , participantId = participantId
                                , amount = task.points
                                , type_ = EarnedFromTask taskId
                                , description = "Completed: " ++ task.title
                                , timestamp = now
                                , approvedBy = Nothing
                                }

                            updatedModel =
                                { model
                                    | nextId = model.nextId + 1
                                    , transactions = Dict.insert newId tx model.transactions
                                }
                        in
                        ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

                _ ->
                    ( model, Lamdera.sendToFrontend clientId (ActionFailed "Invalid participant or task.") )

        RedeemReward { participantId, rewardId } ->
            case ( Dict.get participantId model.participants, Dict.get rewardId model.rewards ) of
                ( Just _, Just reward ) ->
                    let
                        currentBalance =
                            balanceFor participantId model.transactions
                    in
                    if not reward.active then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed "Reward is no longer available.") )

                    else if currentBalance < reward.cost then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed "Not enough points!") )

                    else
                        let
                            tx =
                                { id = newId
                                , participantId = participantId
                                , amount = -reward.cost
                                , type_ = SpentOnReward rewardId
                                , description = "Redeemed: " ++ reward.title
                                , timestamp = now
                                , approvedBy = Nothing
                                }

                            updatedModel =
                                { model
                                    | nextId = model.nextId + 1
                                    , transactions = Dict.insert newId tx model.transactions
                                }
                        in
                        ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

                _ ->
                    ( model, Lamdera.sendToFrontend clientId (ActionFailed "Invalid participant or reward.") )


alreadyClaimedOneTime : ParticipantId -> TaskId -> Dict TransactionId PointTransaction -> Bool
alreadyClaimedOneTime participantId taskId transactions =
    transactions
        |> Dict.values
        |> List.any
            (\tx ->
                tx.participantId == participantId && tx.type_ == EarnedFromTask taskId
            )


snapshotFor : BackendModel -> ToFrontend
snapshotFor model =
    StateSynced
        { participants = model.participants
        , tasks = model.tasks
        , rewards = model.rewards
        , transactions = model.transactions
        }
