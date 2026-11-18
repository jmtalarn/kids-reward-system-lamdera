module Frontend exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict
import Domain exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Lamdera
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
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
      }
    , Lamdera.sendToBackend RequestInitialState
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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        StateSynced data ->
            ( { model
                | participants = data.participants
                , tasks = data.tasks
                , rewards = data.rewards
                , transactions = data.transactions
              }
            , Cmd.none
            )

        ActionFailed err ->
            ( { model | errorMessage = Just err }, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Kids Reward System (Dev Test)"
    , body =
        [ div [ style "font-family" "sans-serif", style "max-width" "640px", style "margin" "2rem auto" ]
            [ h1 [] [ text "Kids Reward System" ]
            , case model.errorMessage of
                Just err ->
                    div [ style "background" "#FEE2E2", style "color" "#991B1B", style "padding" "0.75rem", style "margin-bottom" "1rem" ]
                        [ text err
                        , button [ onClick DismissError, style "margin-left" "1rem" ] [ text "x" ]
                        ]

                Nothing ->
                    text ""
            , h2 [] [ text "Participants" ]
            , div []
                (Dict.values model.participants
                    |> List.map
                        (\p ->
                            div [ style "padding" "0.5rem", style "border" "1px solid #ccc", style "margin-bottom" "0.5rem" ]
                                [ text (p.avatar ++ " " ++ p.name ++ " — Balance: " ++ String.fromInt (balanceFor p.id model.transactions) ++ " pts") ]
                        )
                )
            , h2 [] [ text "Tasks (Click to Complete for Leo)" ]
            , div []
                (Dict.values model.tasks
                    |> List.map
                        (\t ->
                            div [ style "margin-bottom" "0.5rem" ]
                                [ button [ onClick (ClickCompleteTask "1" t.id) ]
                                    [ text ("+" ++ String.fromInt t.points ++ " pts | " ++ t.title ++ " (" ++ Debug.toString t.recurrence ++ ")") ]
                                ]
                        )
                )
            , h2 [] [ text "Shop Rewards" ]
            , div []
                (Dict.values model.rewards
                    |> List.map
                        (\r ->
                            div [ style "margin-bottom" "0.5rem" ]
                                [ button [ onClick (ClickRedeemReward "1" r.id) ]
                                    [ text (r.emoji ++ " " ++ r.title ++ " (-" ++ String.fromInt r.cost ++ " pts)") ]
                                ]
                        )
                )
            , h2 [] [ text "Point Ledger" ]
            , ul []
                (Dict.values model.transactions
                    |> List.reverse
                    |> List.map
                        (\tx ->
                            li [] [ text (tx.description ++ " (" ++ String.fromInt tx.amount ++ " pts)") ]
                        )
                )
            ]
        ]
    }
