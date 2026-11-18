module Frontend exposing (..)

import AdminView exposing (viewAdminPanel)
import Browser
import Browser.Navigation as Nav
import Dict
import Domain exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, style, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Css exposing (..)
import Styles
import Lamdera
import Types exposing (..)
import Url
import KidView exposing (viewKidDashboard, viewLedger, viewParticipantBar)
import Time
import Task




-- Inside view : FrontendModel -> Browser.Document FrontendMsg


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =  \_ -> Time.every 60000 Tick
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
      }
    , Cmd.batch
        [ Lamdera.sendToBackend RequestInitialState
        , Task.perform GotZone Time.here
        , Task.perform Tick Time.now
        ]
    )


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
            ( { model | errorMessage = Nothing }
            , Lamdera.sendToBackend (CompleteTask { participantId = pid, taskId = tid })
            )

        ClickRedeemReward pid rid ->
            ( { model | errorMessage = Nothing }
            , Lamdera.sendToBackend (RedeemReward { participantId = pid, rewardId = rid })
            )

        DismissError ->
            ( { model | errorMessage = Nothing }, Cmd.none )

        UpdatePinInput val ->
            ( { model | pinInput = val }, Cmd.none )

        SubmitPin ->
            ( { model | errorMessage = Nothing }
            , Lamdera.sendToBackend (VerifyParentPin model.pinInput)
            )

        LockParentMode ->
            ( { model | isParentMode = False, pinInput = "" }, Lamdera.sendToBackend LogoutParent )

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
            case ( String.trim model.newTaskTitle, String.toInt model.newTaskPoints ) of
                ( "", _ ) ->
                    ( { model | errorMessage = Just "Enter a task title." }, Cmd.none )

                ( title, Just pts ) ->
                    ( { model | newTaskTitle = "", newTaskPoints = "10", newTaskAssignedTo = [] }
                    , Lamdera.sendToBackend
                        (CreateTask
                            { title = title
                            , description = ""
                            , points = pts
                            , recurrence = model.newTaskRecurrence
                            , assignedTo = model.newTaskAssignedTo
                            }
                        )
                    )

                _ ->
                    ( { model | errorMessage = Just "Points must be a valid number." }, Cmd.none )


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
            ( { model | now = t }, Cmd.none )

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
              }
            , Cmd.none
            )

        ParentPinVerified ok ->
            ( { model | isParentMode = ok, pinInput = "" }, Cmd.none )

        ActionFailed err ->
            ( { model | errorMessage = Just err }, Cmd.none )

view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Kids Reward System"
    , body = [ toUnstyled (viewPage model) ]
    }

viewPage : FrontendModel -> Html FrontendMsg
viewPage model =
    div [ Styles.pageShell ]
        [ div [ Styles.container ]
            [ viewHeader model
            , viewError model.errorMessage
            , viewParticipantBar model
            , case model.selectedParticipant |> Maybe.andThen (\pid -> Dict.get pid model.participants) of
                Just kid ->
                    viewKidDashboard kid model

                Nothing ->
                    p [] [ text "No kids yet. Unlock Parent Mode to add one!" ]
            , if model.isParentMode then
                viewAdminPanel model

              else
                text ""
            , viewLedger model
            ]
        ]


viewHeader : FrontendModel -> Html FrontendMsg
viewHeader model =
    div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, marginBottom (rem 1.25) ] ]
        [ h1 [ css [ margin zero, fontSize (rem 1.6) ] ] [ text "Kids Reward System" ]
        , if model.isParentMode then
            button [ onClick LockParentMode, Styles.actionButton True "475569" ] [ text "🔒 Lock Parent" ]

          else
            form [ onSubmit SubmitPin, Styles.row 0.5 ]
                [ input
                    [ type_ "password"
                    , placeholder "PIN"
                    , value model.pinInput
                    , onInput UpdatePinInput
                    , css [ width (px 80), padding (rem 0.5), borderRadius (px 8), border3 (px 1) solid (hex "CBD5E1") ]
                    ]
                    []
                , button [ type_ "submit", Styles.actionButton True "2563EB" ] [ text "Parent Mode" ]
                ]
        ]


viewError : Maybe String -> Html FrontendMsg
viewError maybeErr =
    case maybeErr of
        Just err ->
            div [ css [ backgroundColor (hex "FEE2E2"), color (hex "991B1B"), padding (rem 0.75), borderRadius (px 10), marginBottom (rem 1) ] ]
                [ text err
                , button [ onClick DismissError, css [ marginLeft (rem 1) ] ] [ text "x" ]
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
        [ h2 [] [ text "Tasks" ]
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
                                        ++ Debug.toString t.recurrence
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
        [ h2 [] [ text "Reward Shop" ]
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


viewLedger : FrontendModel -> Html FrontendMsg
viewLedger model =
    div []
        [ h2 [] [ text "Point Ledger" ]
        , ul []
            (Dict.values model.transactions
                |> List.reverse
                |> List.map
                    (\tx ->
                        let
                            kidName =
                                Dict.get tx.participantId model.participants
                                    |> Maybe.map (\p -> p.avatar ++ " " ++ p.name)
                                    |> Maybe.withDefault tx.participantId
                        in
                        li [] [ text (kidName ++ ": " ++ tx.description ++ " (" ++ String.fromInt tx.amount ++ " pts)") ]
                    )
            )
        ]
