module Backend exposing (..)

import Dict exposing (Dict)
import Domain exposing (..)
import Env
import Lamdera exposing (ClientId, SessionId)
import Task
import Time
import Types exposing (..)
import Set exposing (Set)

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
      , authenticatedSessions = Set.empty
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
updateFromFrontend sessionId clientId msg model =
    case msg of
        RequestInitialState ->
            ( model
            , Lamdera.sendToFrontend clientId (snapshotFor model)
            )

        VerifyParentPin attempt ->
            if attempt == Env.parentPin then
                ( { model | authenticatedSessions = Set.insert sessionId model.authenticatedSessions }
                , Lamdera.sendToFrontend clientId (ParentPinVerified True)
                )

            else
                ( model, Lamdera.sendToFrontend clientId (ActionFailed "Incorrect Parent PIN.") )

        LogoutParent ->
            ( { model | authenticatedSessions = Set.remove sessionId model.authenticatedSessions }
            , Lamdera.sendToFrontend clientId (ParentPinVerified False)
            )

        CompleteTask _ ->
            ( model, Task.perform (GotTimeForToBackend clientId msg) Time.now )

        RedeemReward _ ->
            ( model, Task.perform (GotTimeForToBackend clientId msg) Time.now )

        _ ->
            if Set.member sessionId model.authenticatedSessions then
                ( model, Task.perform (GotTimeForToBackend clientId msg) Time.now )

            else
                ( model, Lamdera.sendToFrontend clientId (ActionFailed "Parent authorization required.") )



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

        VerifyParentPin _ ->
            ( model, Cmd.none )

        AddManualAdjustment { participantId, amount, reason } ->
            case Dict.get participantId model.participants of
                Just _ ->
                    let
                        tx =
                            { id = newId
                            , participantId = participantId
                            , amount = amount
                            , type_ = ManualAdjustment reason
                            , description =
                                if String.isEmpty (String.trim reason) then
                                    "Parent adjustment"

                                else
                                    reason
                            , timestamp = now
                            , approvedBy = Just "Parent"
                            }

                        updatedModel =
                            { model
                                | nextId = model.nextId + 1
                                , transactions = Dict.insert newId tx model.transactions
                            }
                    in
                    ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId (ActionFailed "Participant not found.") )
        
        CompleteTask { participantId, taskId, forDate } ->
            case ( Dict.get participantId model.participants, Dict.get taskId model.tasks ) of
                ( Just _, Just task ) ->
                    let
                        isFuture =
                            Time.posixToMillis forDate > (Time.posixToMillis now + 86400000)
                    in
                    if not task.active then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed "This task is inactive.") )

                    else if isFuture then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed "Cannot complete tasks for future dates!") )

                    else if not (canClaimTask serverZone forDate participantId task model.transactions) then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed (cooldownMessage task.recurrence)) )

                    else
                        let
                            tx =
                                { id = newId
                                , participantId = participantId
                                , amount = task.points
                                , type_ = EarnedFromTask taskId
                                , description = "Completed: " ++ task.title
                                , timestamp = forDate
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


        LogoutParent ->
            ( model, Cmd.none )

        ToggleTaskActive taskId ->
            let
                updatedTasks =
                    Dict.update taskId (Maybe.map (\t -> { t | active = not t.active })) model.tasks

                updatedModel =
                    { model | tasks = updatedTasks }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

        ToggleRewardActive rewardId ->
            let
                updatedRewards =
                    Dict.update rewardId (Maybe.map (\r -> { r | active = not r.active })) model.rewards

                updatedModel =
                    { model | rewards = updatedRewards }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )
        DeleteTransaction txId ->
            let
                updatedModel =
                    { model | transactions = Dict.remove txId model.transactions }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )
        UpdateParticipant data ->
            let
                updated =
                    Dict.update data.id
                        (Maybe.map (\p -> { p | name = data.name, avatar = data.avatar, color = data.color }))
                        model.participants

                updatedModel =
                    { model | participants = updated }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

        UpdateTask data ->
            let
                updated =
                    Dict.update data.id
                        (Maybe.map
                            (\t ->
                                { t
                                    | title = data.title
                                    , description = data.description
                                    , points = data.points
                                    , recurrence = data.recurrence
                                    , assignedTo = data.assignedTo
                                }
                            )
                        )
                        model.tasks

                updatedModel =
                    { model | tasks = updated }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

        UpdateReward data ->
            let
                updated =
                    Dict.update data.id
                        (Maybe.map
                            (\r ->
                                { r
                                    | title = data.title
                                    , description = data.description
                                    , cost = data.cost
                                    , emoji = data.emoji
                                }
                            )
                        )
                        model.rewards

                updatedModel =
                    { model | rewards = updated }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

cooldownMessage : Recurrence -> String
cooldownMessage recurrence =
    case recurrence of
        OneTime ->
            "One-time task already completed!"

        Daily ->
            "Already completed today! Come back tomorrow."

        Weekly ->
            "Already completed this week! Resets Monday."

        Monthly ->
            "Already completed this month!"
        
        SpecificDays _ ->
            "Not scheduled for this day (or already completed today)!"

snapshotFor : BackendModel -> ToFrontend
snapshotFor model =
    StateSynced
        { participants = model.participants
        , tasks = model.tasks
        , rewards = model.rewards
        , transactions = model.transactions
        , pinLength = String.length Env.parentPin
        }

serverZone : Time.Zone
serverZone =
    Time.customZone Env.timezoneOffsetMinutes []