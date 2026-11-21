module Backend exposing (..)

import Dict
import Domain exposing (..)
import Env
import I18n
import Lamdera exposing (ClientId, SessionId)
import Set
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
            , goalRewardIds = []
            , active = True
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
            , requiresApproval = False
            , startDate = Nothing
            , endDate = Nothing
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
            , requiresApproval = False
            , startDate = Nothing
            , endDate = Nothing
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
      , lang = En
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
                ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.incorrectPinError model.lang)) )

        LogoutParent ->
            ( { model | authenticatedSessions = Set.remove sessionId model.authenticatedSessions }
            , Lamdera.sendToFrontend clientId (ParentPinVerified False)
            )

        CompleteTask _ ->
            ( model, Task.perform (GotTimeForToBackend clientId msg) Time.now )

        RedeemReward _ ->
            ( model, Task.perform (GotTimeForToBackend clientId msg) Time.now )

        ToggleParticipantGoal _ ->
                ( model, Task.perform (GotTimeForToBackend clientId msg) Time.now )
        _ ->
            if Set.member sessionId model.authenticatedSessions then
                ( model, Task.perform (GotTimeForToBackend clientId msg) Time.now )

            else
                ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.parentAuthorizationError model.lang)) )


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
                    , goalRewardIds = []
                    , active = True
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
                    , requiresApproval = data.requiresApproval
                    , startDate = data.startDate
                    , endDate = data.endDate
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
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.rewardUnavailableError model.lang)) )

                    else if currentBalance < reward.cost then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.notEnoughPointsError model.lang)) )

                    else
                        let
                            tx =
                                { id = newId
                                , participantId = participantId
                                , amount = -reward.cost
                                , type_ = SpentOnReward rewardId
                                , description = I18n.redeemedDescription model.lang reward.title
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
                    ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.invalidParticipantOrTaskError model.lang)) )

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
                                    I18n.parentAdjustmentDescription model.lang

                                else
                                    reason
                            , timestamp = now
                            , approvedBy = Just (I18n.parentApprovedLabel model.lang)
                            }

                        updatedModel =
                            { model
                                | nextId = model.nextId + 1
                                , transactions = Dict.insert newId tx model.transactions
                            }
                    in
                    ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.participantNotFoundError model.lang)) )

        CompleteTask { participantId, taskId, forDate } ->
            case ( Dict.get participantId model.participants, Dict.get taskId model.tasks ) of
                ( Just _, Just task ) ->
                    let
                        isFuture =
                            Time.posixToMillis forDate > (Time.posixToMillis now + 86400000)
                    in
                    if not task.active then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.taskInactiveError model.lang)) )

                    else if isFuture then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.futureTaskError model.lang)) )
                    
                    else if not (isTaskActiveOnDate serverZone forDate task) then
                     ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.unscheduledTaskError model.lang)) )

                    else if not (canClaimTask serverZone forDate participantId task model.transactions) then
                        ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.alreadyCompletedError model.lang task.recurrence)) )

                    else
                        let
                            tx =
                                { id = newId
                                , participantId = participantId
                                , amount = task.points
                                , type_ = EarnedFromTask taskId
                                , description = I18n.completedDescription model.lang task.title
                                , timestamp = forDate
                                , approvedBy =
                                    if task.requiresApproval then
                                        Nothing

                                    else
                                        Just (I18n.autoApprovedLabel model.lang)
                                }

                            updatedModel =
                                { model
                                    | nextId = model.nextId + 1
                                    , transactions = Dict.insert newId tx model.transactions
                                }
                        in
                        ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

                _ ->
                    ( model, Lamdera.sendToFrontend clientId (ActionFailed (I18n.invalidParticipantOrRewardError model.lang)) )

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
                                    , requiresApproval = data.requiresApproval
                                    , startDate = data.startDate
                                    , endDate = data.endDate
                                }
                            )
                        )
                        model.tasks

                updatedModel =
                    { model | tasks = updated }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )
       
        ToggleParticipantGoal { participantId, rewardId } ->
                    let
                        toggleGoal p =
                            let
                                nextGoals =
                                    if List.member rewardId p.goalRewardIds then
                                        List.filter ((/=) rewardId) p.goalRewardIds

                                    else
                                        p.goalRewardIds ++ [ rewardId ]
                            in
                            { p | goalRewardIds = nextGoals }

                        updatedModel =
                            { model | participants = Dict.update participantId (Maybe.map toggleGoal) model.participants }
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

        ApproveTransaction txId ->
            let
                updated =
                    Dict.update txId (Maybe.map (\tx -> { tx | approvedBy = Just (I18n.parentApprovedLabel model.lang) })) model.transactions

                updatedModel =
                    { model | transactions = updated }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )

        RejectTransaction txId ->
            let
                updatedModel =
                    { model | transactions = Dict.remove txId model.transactions }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )
        UpdateLanguage newLang ->
            let
                updatedModel =
                    { model | lang = newLang }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )
        ToggleParticipantActive pid ->
            let
                updated =
                    Dict.update pid (Maybe.map (\p -> { p | active = not p.active })) model.participants

                updatedModel =
                    { model | participants = updated }
            in
            ( updatedModel, Lamdera.broadcast (snapshotFor updatedModel) )


snapshotFor : BackendModel -> ToFrontend
snapshotFor model =
    StateSynced
        { participants = model.participants
        , tasks = model.tasks
        , rewards = model.rewards
        , transactions = model.transactions
        , pinLength = String.length Env.parentPin
        , lang = model.lang
        }


serverZone : Time.Zone
serverZone =
    Time.customZone Env.timezoneOffsetMinutes []
