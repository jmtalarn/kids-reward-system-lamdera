module Frontend exposing (..)

import AdminView exposing (viewAdminPanel)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import CalendarView exposing (viewCalendarTab)
import CelebrationView exposing (viewCelebration)
import Css exposing (..)
import Dict
import Domain exposing (..)
import EmojiPickerModal exposing (viewEmojiPickerModal)
import Env
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, style)
import Html.Styled.Events exposing (onClick)
import I18n
import Json.Decode as Decode
import KidView exposing (viewHeroBanner, viewLedger, viewParticipantBar, viewRewardShop, viewTaskGrid)
import Lamdera
import PinPadModal exposing (viewPinPadModal)
import Process
import Styles
import Task
import Time
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , url = url
      , participants = Dict.empty
      , tasks = Dict.empty
      , rewards = Dict.empty
      , transactions = Dict.empty
      , selectedParticipant = Just "1"
      , errorMessage = Nothing
      , now = Time.millisToPosix 0
      , zone = Time.utc
      , isParentMode = False
      , pinInput = ""
      , newKidName = ""
      , newKidAvatar = "🦊"
      , newTaskTitle = ""
      , newTaskPoints = "10"
      , newTaskRecurrence = Daily
      , newRewardTitle = ""
      , newRewardCost = "30"
      , newRewardEmoji = "🍦"
      , bonusAmount = "5"
      , bonusReason = ""
      , newKidColor = "#3B82F6"
      , newTaskAssignedTo = []
      , activeTab = TasksTab
      , celebration = Nothing
      , celebrationCount = 0
      , selectedDate = Time.millisToPosix 0
      , lang = En
      , newTaskDays = [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri ]
      , editingKidId = Nothing
      , editingTaskId = Nothing
      , editingRewardId = Nothing
      , showPinPad = False
      , pinLength = String.length Env.parentPin
      , emojiPicker = Nothing
      , newTaskRequiresApproval = False
      , newTaskStartDate = ""
      , newTaskEndDate = ""
      }
    , Cmd.batch
        [ Lamdera.sendToBackend RequestInitialState
        , Task.perform GotZone Time.here
        , Task.perform Tick Time.now
        ]
    )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Time.every 60000 Tick
        , if model.showPinPad then
            Browser.Events.onKeyDown pinKeyDecoder

          else
            Sub.none
        ]


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        SelectParticipant maybeId ->
            ( { model | selectedParticipant = maybeId }, Cmd.none )

        ClickCompleteTask pid tid ->
            update (ClickCompleteTaskForDate pid tid model.now) model

        ClickCompleteTaskForDate pid tid targetDate ->
            let
                nextCount =
                    model.celebrationCount + 1

                maybeTask =
                    Dict.get tid model.tasks

                needsApproval =
                    maybeTask |> Maybe.map .requiresApproval |> Maybe.withDefault False

                cel =
                    if needsApproval then
                        Nothing

                    else
                        maybeTask
                            |> Maybe.map
                                (\t ->
                                    { id = nextCount
                                    , title = "🎉 +" ++ String.fromInt t.points ++ " Points!"
                                    , subtitle = t.title
                                    , isReward = False
                                    }
                                )
            in
            ( { model
                | errorMessage =
                    if needsApproval then
                        Just "⏳ Sent for Parent Approval!"

                    else
                        Nothing
                , celebration = cel
                , celebrationCount = nextCount
              }
            , Cmd.batch
                [ Lamdera.sendToBackend
                    (CompleteTask
                        { participantId = pid
                        , taskId = tid
                        , forDate = targetDate
                        }
                    )
                , if needsApproval then
                    Cmd.none

                  else
                    Process.sleep 3500 |> Task.perform (\_ -> ClearCelebration nextCount)
                ]
            )

        ClickRedeemReward pid rid ->
            let
                nextCount =
                    model.celebrationCount + 1

                maybeReward =
                    Dict.get rid model.rewards

                cel =
                    maybeReward
                        |> Maybe.map
                            (\r ->
                                { id = nextCount
                                , title = r.emoji ++ " Reward Unlocked!"
                                , subtitle = r.title
                                , isReward = True
                                }
                            )
            in
            ( { model | errorMessage = Nothing, celebration = cel, celebrationCount = nextCount }
            , Cmd.batch
                [ Lamdera.sendToBackend (RedeemReward { participantId = pid, rewardId = rid })
                , Process.sleep 3500 |> Task.perform (\_ -> ClearCelebration nextCount)
                ]
            )

        ClearCelebration id ->
            if Maybe.map .id model.celebration == Just id then
                ( { model | celebration = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        DismissError ->
            ( { model | errorMessage = Nothing }, Cmd.none )

        UpdatePinInput val ->
            ( { model | pinInput = val }, Cmd.none )

        SubmitPin ->
            ( { model | errorMessage = Nothing }
            , Lamdera.sendToBackend (VerifyParentPin model.pinInput)
            )

        LockParentMode ->
            ( { model
                | isParentMode = False
                , pinInput = ""
                , activeTab = TasksTab
              }
            , Lamdera.sendToBackend LogoutParent
            )

        UpdateNewKidName val ->
            ( { model | newKidName = val }, Cmd.none )

        UpdateNewKidAvatar val ->
            ( { model | newKidAvatar = val }, Cmd.none )

        SubmitNewKid ->
            if String.isEmpty (String.trim model.newKidName) then
                ( { model | errorMessage = Just "Enter a name for the kid." }, Cmd.none )

            else
                ( { model | newKidName = "" }
                , Lamdera.sendToBackend
                    (CreateParticipant
                        { name = String.trim model.newKidName
                        , avatar = model.newKidAvatar
                        , color = model.newKidColor
                        }
                    )
                )

        UpdateNewTaskTitle val ->
            ( { model | newTaskTitle = val }, Cmd.none )

        UpdateNewTaskPoints val ->
            ( { model | newTaskPoints = val }, Cmd.none )

        UpdateNewTaskRecurrence rec ->
            ( { model | newTaskRecurrence = rec }, Cmd.none )

        SubmitNewTask ->
            let
                finalRecurrence =
                    case model.newTaskRecurrence of
                        SpecificDays _ ->
                            SpecificDays model.newTaskDays

                        other ->
                            other

                hasEmptyDays =
                    case finalRecurrence of
                        SpecificDays days ->
                            List.isEmpty days

                        _ ->
                            False
            in
            case ( String.trim model.newTaskTitle, String.toInt model.newTaskPoints ) of
                ( "", _ ) ->
                    ( { model | errorMessage = Just "Enter a task title." }, Cmd.none )

                ( _, Nothing ) ->
                    ( { model | errorMessage = Just "Points must be a valid number." }, Cmd.none )

                ( title, Just pts ) ->
                    if hasEmptyDays then
                        ( { model | errorMessage = Just "Select at least one day." }, Cmd.none )

                    else
                        let
                            parsedStart =
                                parseDateInput model.newTaskStartDate

                            parsedEnd =
                                parseDateInput model.newTaskEndDate

                            cmd =
                                case model.editingTaskId of
                                    Just tid ->
                                        Lamdera.sendToBackend
                                            (UpdateTask
                                                { id = tid
                                                , title = title
                                                , description = ""
                                                , points = pts
                                                , recurrence = finalRecurrence
                                                , assignedTo = model.newTaskAssignedTo
                                                , requiresApproval = model.newTaskRequiresApproval
                                                , startDate = parsedStart
                                                , endDate = parsedEnd
                                                }
                                            )

                                    Nothing ->
                                        Lamdera.sendToBackend
                                            (CreateTask
                                                { title = title
                                                , description = ""
                                                , points = pts
                                                , recurrence = finalRecurrence
                                                , assignedTo = model.newTaskAssignedTo
                                                , requiresApproval = model.newTaskRequiresApproval
                                                , startDate = parsedStart
                                                , endDate = parsedEnd
                                                }
                                            )
                        in
                        ( { model
                            | editingTaskId = Nothing
                            , newTaskTitle = ""
                            , newTaskPoints = "10"
                            , newTaskDays = []
                            , newTaskAssignedTo = []
                            , newTaskRequiresApproval = False
                            , newTaskStartDate = ""
                            , newTaskEndDate = ""
                          }
                        , cmd
                        )

        ToggleTaskDay day ->
            let
                updatedDays =
                    if List.member day model.newTaskDays then
                        List.filter ((/=) day) model.newTaskDays

                    else
                        model.newTaskDays ++ [ day ]
            in
            ( { model | newTaskDays = updatedDays }, Cmd.none )

        SetTaskDays days ->
            ( { model | newTaskDays = days }, Cmd.none )

        UpdateNewRewardTitle val ->
            ( { model | newRewardTitle = val }, Cmd.none )

        UpdateNewRewardCost val ->
            ( { model | newRewardCost = val }, Cmd.none )

        UpdateNewRewardEmoji val ->
            ( { model | newRewardEmoji = val }, Cmd.none )

        SubmitNewReward ->
            case ( String.trim model.newRewardTitle, String.toInt model.newRewardCost ) of
                ( "", _ ) ->
                    ( { model | errorMessage = Just "Enter a reward title." }, Cmd.none )

                ( title, Just cost ) ->
                    ( { model | newRewardTitle = "", newRewardCost = "30" }
                    , Lamdera.sendToBackend
                        (CreateReward
                            { title = title
                            , description = ""
                            , cost = cost
                            , emoji = model.newRewardEmoji
                            }
                        )
                    )

                _ ->
                    ( { model | errorMessage = Just "Cost must be a valid number." }, Cmd.none )

        UpdateBonusAmount val ->
            ( { model | bonusAmount = val }, Cmd.none )

        UpdateBonusReason val ->
            ( { model | bonusReason = val }, Cmd.none )

        SubmitBonus pid ->
            case String.toInt model.bonusAmount of
                Just amt ->
                    ( { model | bonusReason = "" }
                    , Lamdera.sendToBackend
                        (AddManualAdjustment
                            { participantId = pid
                            , amount = amt
                            , reason = model.bonusReason
                            }
                        )
                    )

                Nothing ->
                    ( { model | errorMessage = Just "Adjustment must be a number." }, Cmd.none )

        SelectNewKidColor hexColor ->
            ( { model | newKidColor = hexColor }, Cmd.none )

        ToggleTaskAssignee pid ->
            let
                updatedList =
                    if List.member pid model.newTaskAssignedTo then
                        List.filter ((/=) pid) model.newTaskAssignedTo

                    else
                        pid :: model.newTaskAssignedTo
            in
            ( { model | newTaskAssignedTo = updatedList }, Cmd.none )

        ClickToggleTaskActive tid ->
            ( model
            , Lamdera.sendToBackend (ToggleTaskActive tid)
            )

        ClickToggleRewardActive rid ->
            ( model
            , Lamdera.sendToBackend (ToggleRewardActive rid)
            )

        GotZone z ->
            ( { model | zone = z }, Cmd.none )

        Tick t ->
            ( { model
                | now = t
                , selectedDate =
                    if Time.posixToMillis model.selectedDate == 0 then
                        t

                    else
                        model.selectedDate
              }
            , Cmd.none
            )

        SelectTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        SelectCalendarDate date ->
            ( { model | selectedDate = date }, Cmd.none )

        ClickDeleteTransaction txId ->
            ( model
            , Lamdera.sendToBackend (DeleteTransaction txId)
            )

        SetLanguage newLang ->
            ( { model | lang = newLang }, Lamdera.sendToBackend (UpdateLanguage newLang) )

        StartEditKid kid ->
            ( { model | editingKidId = Just kid.id, newKidName = kid.name, newKidAvatar = kid.avatar, newKidColor = kid.color }
            , Task.attempt (\_ -> NoOp) (Dom.focus "edit-kid-input")
            )

        CancelEditKid ->
            ( { model | editingKidId = Nothing, newKidName = "", newKidAvatar = "🦊" }, Cmd.none )

        StartEditTask task ->
            let
                days =
                    case task.recurrence of
                        SpecificDays d ->
                            d

                        _ ->
                            []
            in
            ( { model
                | editingTaskId = Just task.id
                , newTaskTitle = task.title
                , newTaskPoints = String.fromInt task.points
                , newTaskRecurrence = task.recurrence
                , newTaskDays = days
                , newTaskAssignedTo = task.assignedTo
                , newTaskRequiresApproval = task.requiresApproval
                , newTaskStartDate = Maybe.map (formatDateInput model.zone) task.startDate |> Maybe.withDefault ""
                , newTaskEndDate = Maybe.map (formatDateInput model.zone) task.endDate |> Maybe.withDefault ""
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "edit-task-input")
            )

        CancelEditTask ->
            ( { model | editingTaskId = Nothing, newTaskTitle = "", newTaskPoints = "10", newTaskDays = [], newTaskAssignedTo = [], newTaskRequiresApproval = False, newTaskStartDate = "", newTaskEndDate = "" }, Cmd.none )

        StartEditReward reward ->
            ( { model | editingRewardId = Just reward.id, newRewardTitle = reward.title, newRewardCost = String.fromInt reward.cost, newRewardEmoji = reward.emoji }
            , Task.attempt (\_ -> NoOp) (Dom.focus "edit-reward-input")
            )

        CancelEditReward ->
            ( { model | editingRewardId = Nothing, newRewardTitle = "", newRewardCost = "30", newRewardEmoji = "🍦" }, Cmd.none )

        OpenPinPad ->
            ( { model | showPinPad = True, pinInput = "", errorMessage = Nothing }, Cmd.none )

        ClosePinPad ->
            ( { model | showPinPad = False, pinInput = "" }, Cmd.none )

        PressPinDigit digit ->
            let
                nextPin =
                    model.pinInput ++ digit
            in
            if String.length nextPin >= model.pinLength then
                ( { model | errorMessage = Nothing, pinInput = "" }
                , Lamdera.sendToBackend (VerifyParentPin nextPin)
                )

            else
                ( { model | errorMessage = Nothing, pinInput = nextPin }, Cmd.none )

        BackspacePin ->
            ( { model | errorMessage = Nothing, pinInput = String.dropRight 1 model.pinInput }
            , Cmd.none
            )

        ClickToggleGoal pid rid ->
            ( model
            , Lamdera.sendToBackend (ToggleParticipantGoal { participantId = pid, rewardId = rid })
            )

        NoOp ->
            ( model, Cmd.none )

        OpenEmojiPicker target ->
            ( { model | emojiPicker = Just target }, Cmd.none )

        CloseEmojiPicker ->
            ( { model | emojiPicker = Nothing }, Cmd.none )

        PickEmoji emojiStr ->
            case model.emojiPicker of
                Just KidEmojiTarget ->
                    ( { model | newKidAvatar = emojiStr, emojiPicker = Nothing }, Cmd.none )

                Just RewardEmojiTarget ->
                    ( { model | newRewardEmoji = emojiStr, emojiPicker = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleNewTaskRequiresApproval ->
            ( { model | newTaskRequiresApproval = not model.newTaskRequiresApproval }, Cmd.none )

        ClickApproveTransaction txId ->
            ( model, Lamdera.sendToBackend (ApproveTransaction txId) )

        ClickRejectTransaction txId ->
            ( model, Lamdera.sendToBackend (RejectTransaction txId) )

        UpdateNewTaskStartDate val ->
            ( { model | newTaskStartDate = val }, Cmd.none )

        UpdateNewTaskEndDate val ->
            ( { model | newTaskEndDate = val }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        StateSynced data ->
            let
                activeKid =
                    case model.selectedParticipant of
                        Just id ->
                            if Dict.member id data.participants then
                                Just id

                            else
                                Dict.keys data.participants |> List.head

                        Nothing ->
                            Dict.keys data.participants |> List.head
            in
            ( { model
                | participants = data.participants
                , tasks = data.tasks
                , rewards = data.rewards
                , transactions = data.transactions
                , selectedParticipant = activeKid
                , lang = data.lang
              }
            , Cmd.none
            )

        ParentPinVerified ok ->
            ( { model
                | isParentMode = ok
                , showPinPad = False
                , pinInput = ""
                , activeTab =
                    if ok then
                        AdminTab

                    else
                        TasksTab
              }
            , Cmd.none
            )

        ActionFailed err ->
            ( { model | errorMessage = Just err }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Kids Reward System"
    , body = [ toUnstyled (viewPage model) ]
    }


viewPage : FrontendModel -> Html FrontendMsg
viewPage model =
    div [ Styles.pageShell, css [ paddingBottom (rem 6) ] ]
        [ div [ Styles.container ]
            [ viewHeader model
            , viewParticipantBar model
            , viewActiveTab model
            ]
        , viewError model
        , viewPinPadModal model
        , viewCelebration model.celebration
        , viewEmojiPickerModal model.lang model.emojiPicker
        , viewBottomTabs model
        ]


viewActiveTab : FrontendModel -> Html FrontendMsg
viewActiveTab model =
    let
        maybeKid =
            model.selectedParticipant
                |> Maybe.andThen (\pid -> Dict.get pid model.participants)
    in
    case model.activeTab of
        TasksTab ->
            case maybeKid of
                Just kid ->
                    let
                        pts =
                            balanceFor kid.id model.transactions

                        streak =
                            dailyStreakFor model.zone model.now kid.id model.tasks model.transactions
                    in
                    div []
                        [ viewHeroBanner model.lang kid pts streak model.rewards
                        , viewTaskGrid model.lang kid model
                        ]

                Nothing ->
                    p [] [ text (I18n.noKidsMessage model.lang) ]

        CalendarTab ->
            case maybeKid of
                Just kid ->
                    viewCalendarTab kid model

                Nothing ->
                    p [] [ text (I18n.selectOrAddKid model.lang) ]

        ShopTab ->
            case maybeKid of
                Just kid ->
                    let
                        pts =
                            balanceFor kid.id model.transactions

                        streak =
                            dailyStreakFor model.zone model.now kid.id model.tasks model.transactions
                    in
                    div []
                        [ viewHeroBanner model.lang kid pts streak model.rewards
                        , viewRewardShop kid pts model
                        ]

                Nothing ->
                    p [] [ text "Select or add a kid first." ]

        ActivityTab ->
            viewLedger model

        AdminTab ->
            if model.isParentMode then
                viewAdminPanel model

            else
                div [ Styles.card [ textAlign center ] ]
                    [ p [] [ text (I18n.unlockParentMode model.lang) ] ]


viewBottomTabs : FrontendModel -> Html FrontendMsg
viewBottomTabs model =
    let
        labels =
            I18n.tabLabels model.lang

        pendingCount =
            pendingApprovalCount model

        parentLabel =
            if pendingCount > 0 then
                labels.parent ++ " (" ++ String.fromInt pendingCount ++ ")"

            else
                labels.parent

        tabs =
            [ ( TasksTab, "🎯", labels.missions )
            , ( CalendarTab, "📅", labels.calendar )
            , ( ShopTab, "🎁", labels.shop )
            , ( ActivityTab, "📜", labels.activity )
            ]
                ++ (if model.isParentMode then
                        [ ( AdminTab, "🛠️", parentLabel ) ]

                    else
                        []
                   )
    in
    nav
        [ css
            [ position fixed
            , bottom zero
            , left zero
            , right zero
            , backgroundColor (hex "FFFFFF")
            , borderTop3 (px 1) solid (hex "E2E8F0")
            , displayFlex
            , justifyContent spaceAround
            , padding2 (rem 0.5) zero
            , zIndex (int 900)
            ]
        ]
        (List.map (viewTabButton model.activeTab) tabs)


viewTabButton : Tab -> ( Tab, String, String ) -> Html FrontendMsg
viewTabButton currentTab ( tab, icon, labelText ) =
    let
        isActive =
            currentTab == tab
    in
    button
        [ onClick (SelectTab tab)
        , css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , property "gap" "0.2rem"
            , padding2 (rem 0.45) (rem 1)
            , borderRadius (px 12)
            , border zero
            , cursor pointer
            , fontWeight (int 700)
            , fontSize (rem 0.8)
            , backgroundColor
                (if isActive then
                    hex "EFF6FF"

                 else
                    rgba 0 0 0 0
                )
            , color
                (if isActive then
                    hex "2563EB"

                 else
                    hex "64748B"
                )
            ]
        ]
        [ span [ css [ fontSize (rem 1.3) ] ] [ text icon ]
        , span [] [ text labelText ]
        ]


viewHeader : FrontendModel -> Html FrontendMsg
viewHeader model =
    let
        pendingCount =
            pendingApprovalCount model

        pendingBadge =
            if pendingCount > 0 then
                span
                    [ css
                        [ position absolute
                        , top (px -8)
                        , right (px -8)
                        , minWidth (px 22)
                        , height (px 22)

                        --, padding2 zero (px 6)
                        , borderRadius (pct 50)
                        , backgroundColor (hex "EF4444")
                        , color (hex "FFFFFF")
                        , border3 (px 1) solid (hex "FFFFFF")
                        , displayFlex
                        , alignItems center
                        , justifyContent center
                        , fontSize (rem 0.72)
                        , fontWeight (int 800)
                        , boxShadow4 zero (px 2) (px 6) (rgba 15 23 42 0.25)
                        ]
                    ]
                    [ text (String.fromInt pendingCount) ]

            else
                text ""
    in
    div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, marginBottom (rem 1.25) ] ]
        [ h1 [ css [ margin zero, fontSize (rem 1.5) ] ] [ text (I18n.appTitle model.lang) ]
        , div [ css [ position relative, display inlineBlock ] ]
            [ if model.isParentMode then
                button [ onClick LockParentMode, Styles.actionButton True "475569" ] [ text (I18n.lockButtonLabel model.lang) ]

              else
                button [ onClick OpenPinPad, Styles.actionButton True "2563EB" ] [ text (I18n.parentButtonLabel model.lang) ]
            , pendingBadge
            ]
        ]


viewError : FrontendModel -> Html FrontendMsg
viewError model =
    if model.showPinPad then
        text ""

    else
        case model.errorMessage of
            Just err ->
                div
                    [ onClick DismissError
                    , css
                        [ position fixed
                        , bottom (rem 5.5)
                        , left (pct 50)
                        , transform (translateX (pct -50))
                        , zIndex (int 1200)
                        , backgroundColor (hex "1E293B")
                        , color (hex "FFFFFF")
                        , padding2 (rem 0.75) (rem 1.25)
                        , borderRadius (px 14)
                        , boxShadow4 zero (px 10) (px 25) (rgba 15 23 42 0.25)
                        , displayFlex
                        , alignItems center
                        , property "gap" "0.75rem"
                        , fontSize (rem 0.9)
                        , fontWeight (int 600)
                        , cursor pointer
                        , maxWidth (vw 90)
                        ]
                    ]
                    [ span [] [ text ("⚠️ " ++ err) ]
                    , span [ css [ opacity (num 0.7), fontSize (rem 0.8) ] ] [ text "✕" ]
                    ]

            Nothing ->
                text ""


viewParticipantSwitcher : FrontendModel -> Html FrontendMsg
viewParticipantSwitcher model =
    div [ style "display" "flex", style "gap" "0.5rem", style "margin-bottom" "1.5rem", style "flex-wrap" "wrap" ]
        (Dict.values model.participants
            |> List.map
                (\p ->
                    let
                        isSelected =
                            model.selectedParticipant == Just p.id

                        pts =
                            balanceFor p.id model.transactions
                    in
                    button
                        [ onClick (SelectParticipant (Just p.id))
                        , style "padding" "0.75rem 1rem"
                        , style "border"
                            (if isSelected then
                                "2px solid #2563EB"

                             else
                                "1px solid #CCC"
                            )
                        , style "background"
                            (if isSelected then
                                "#EFF6FF"

                             else
                                "#FFF"
                            )
                        , style "border-radius" "8px"
                        , style "cursor" "pointer"
                        ]
                        [ text (p.avatar ++ " " ++ p.name ++ " (" ++ String.fromInt pts ++ " pts)") ]
                )
        )


viewTasks : ParticipantId -> FrontendModel -> Html FrontendMsg
viewTasks pid model =
    div [ style "margin-bottom" "1.5rem" ]
        [ h2 [] [ text (I18n.tasksSection model.lang) ]
        , div []
            (Dict.values model.tasks
                |> List.filter (\t -> t.active && (List.isEmpty t.assignedTo || List.member pid t.assignedTo))
                |> List.map
                    (\t ->
                        let
                            available =
                                canClaimTask model.zone model.now pid t model.transactions
                        in
                        div [ style "margin-bottom" "0.5rem" ]
                            [ button
                                [ onClick (ClickCompleteTask pid t.id)
                                , Html.Styled.Attributes.disabled (not available)
                                , style "padding" "0.5rem 0.75rem"
                                , style "opacity"
                                    (if available then
                                        "1"

                                     else
                                        "0.55"
                                    )
                                ]
                                [ text
                                    ((if available then
                                        "⬜ +"

                                      else
                                        "✅ +"
                                     )
                                        ++ String.fromInt t.points
                                        ++ " pts | "
                                        ++ t.title
                                        ++ " ("
                                        ++ I18n.recurrenceFormat model.lang t.recurrence
                                        ++ ")"
                                    )
                                ]
                            ]
                    )
            )
        ]


viewRewards : ParticipantId -> FrontendModel -> Html FrontendMsg
viewRewards pid model =
    div [ style "margin-bottom" "1.5rem" ]
        [ h2 [] [ text (I18n.rewardShopSection model.lang) ]
        , div []
            (Dict.values model.rewards
                |> List.filter .active
                |> List.map
                    (\r ->
                        div [ style "margin-bottom" "0.5rem" ]
                            [ button [ onClick (ClickRedeemReward pid r.id), style "padding" "0.5rem 0.75rem" ]
                                [ text (r.emoji ++ " " ++ r.title ++ " (-" ++ String.fromInt r.cost ++ " pts)") ]
                            ]
                    )
            )
        ]


viewLangSwitcher : Language -> Html FrontendMsg
viewLangSwitcher current =
    div [ Styles.row 0.25 ]
        (List.map
            (\( l, code ) ->
                button
                    [ onClick (SetLanguage l)
                    , css
                        [ padding2 (rem 0.3) (rem 0.55)
                        , borderRadius (px 8)
                        , border zero
                        , cursor pointer
                        , fontWeight (int 700)
                        , fontSize (rem 0.78)
                        , backgroundColor
                            (if current == l then
                                hex "2563EB"

                             else
                                hex "E2E8F0"
                            )
                        , color
                            (if current == l then
                                hex "FFFFFF"

                             else
                                hex "475569"
                            )
                        ]
                    ]
                    [ text code ]
            )
            [ ( Ca, "CA" ), ( Es, "ES" ), ( En, "EN" ) ]
        )


pinKeyDecoder : Decode.Decoder FrontendMsg
pinKeyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "0" ->
                        Decode.succeed (PressPinDigit "0")

                    "1" ->
                        Decode.succeed (PressPinDigit "1")

                    "2" ->
                        Decode.succeed (PressPinDigit "2")

                    "3" ->
                        Decode.succeed (PressPinDigit "3")

                    "4" ->
                        Decode.succeed (PressPinDigit "4")

                    "5" ->
                        Decode.succeed (PressPinDigit "5")

                    "6" ->
                        Decode.succeed (PressPinDigit "6")

                    "7" ->
                        Decode.succeed (PressPinDigit "7")

                    "8" ->
                        Decode.succeed (PressPinDigit "8")

                    "9" ->
                        Decode.succeed (PressPinDigit "9")

                    "Backspace" ->
                        Decode.succeed BackspacePin

                    "Escape" ->
                        Decode.succeed ClosePinPad

                    "Enter" ->
                        Decode.succeed SubmitPin

                    _ ->
                        Decode.fail "Ignored key"
            )


pendingApprovalCount : FrontendModel -> Int
pendingApprovalCount model =
    model.transactions
        |> Dict.values
        |> List.filter (\tx -> tx.amount > 0 && tx.approvedBy == Nothing)
        |> List.length


parseDateInput : String -> Maybe Time.Posix
parseDateInput str =
    case String.split "-" (String.trim str) of
        [ yStr, mStr, dStr ] ->
            case ( String.toInt yStr, String.toInt mStr, String.toInt dStr ) of
                ( Just y, Just m, Just d ) ->
                    let
                        -- Civil days since 1970-01-01 (Howard Hinnant algorithm)
                        yAdj =
                            if m <= 2 then
                                y - 1

                            else
                                y

                        era =
                            yAdj // 400

                        yoe =
                            yAdj - era * 400

                        mAdj =
                            if m > 2 then
                                m - 3

                            else
                                m + 9

                        doy =
                            (153 * mAdj + 2) // 5 + d - 1

                        doe =
                            yoe * 365 + yoe // 4 - yoe // 100 + doy

                        daysSinceEpoch =
                            era * 146097 + doe - 719468
                    in
                    -- Noon UTC avoids any timezone boundary shift
                    Just (Time.millisToPosix (daysSinceEpoch * 86400000 + 43200000))

                _ ->
                    Nothing

        _ ->
            Nothing


formatDateInput : Time.Zone -> Time.Posix -> String
formatDateInput zone t =
    let
        y =
            String.fromInt (Time.toYear zone t)

        m =
            case Time.toMonth zone t of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"

        d =
            String.padLeft 2 '0' (String.fromInt (Time.toDay zone t))
    in
    y ++ "-" ++ m ++ "-" ++ d
