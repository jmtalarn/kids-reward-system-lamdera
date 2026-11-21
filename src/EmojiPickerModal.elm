module EmojiPickerModal exposing (viewEmojiPickerModal)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, type_)
import Html.Styled.Events exposing (onClick)
import I18n
import Types exposing (..)


emojiCategories : List ( String, List String )
emojiCategories =
    [ ( "Chores & Jobs"
      , [ "🧹", "🛏️", "🪥", "🚿", "🧺", "🍽️", "🧽", "🗑️", "🌱", "🐕", "🧸", "👕" ]
      )
    , ( "School & Music"
      , [ "📚", "✏️", "🎒", "🧠", "🎹", "🎸", "🎻", "🥁", "🎺", "🎨", "🔬", "💻" ]
      )
    , ( "Sports & Active"
      , [ "⚽", "🏀", "🎾", "🏊", "🚴", "🥋", "🤸", "🛹", "🛼", "⛸️", "🏃", "🏓" ]
      )
    , ( "Animals"
      , [ "🦊", "🦁", "🐼", "🐨", "🐸", "🦄", "🦖", "🐙", "🐶", "🐱", "🐯", "🐵" ]
      )
    , ( "Fun & Rewards"
      , [ "🎮", "🎬", "🍿", "🎢", "🏖️", "🏕️", "🎲", "🧩", "🪁", "🎪", "🎟️", "🥳" ]
      )
    , ( "Treats & Food"
      , [ "🍦", "🍕", "🍔", "🍟", "🍩", "🍪", "🍫", "🍓", "🍉", "🧁", "🥞", "🧃" ]
      )
    , ( "Stars & Heroes"
      , [ "⭐", "🏆", "🚀", "💎", "🔥", "⚡", "🌈", "👑", "🎯", "🦸", "🧚", "🤖" ]
      )
    ]


viewEmojiPickerModal : Language -> Maybe EmojiTarget -> Html FrontendMsg
viewEmojiPickerModal lang maybeTarget =
    case maybeTarget of
        Nothing ->
            text ""

        Just _ ->
            div
                [ css
                    [ position fixed
                    , top zero
                    , left zero
                    , width (vw 100)
                    , height (vh 100)
                    , backgroundColor (rgba 15 23 42 0.55)
                    , displayFlex
                    , alignItems center
                    , justifyContent center
                    , zIndex (int 1150)
                    , padding (rem 1)
                    ]
                ]
                [ div
                    [ css
                        [ backgroundColor (hex "FFFFFF")
                        , borderRadius (px 24)
                        , padding (rem 1.5)
                        , width (pct 100)
                        , maxWidth (px 380)
                        , maxHeight (vh 80)
                        , property "overflow-y" "auto"
                        , boxShadow4 zero (px 20) (px 40) (rgba 15 23 42 0.25)
                        ]
                    ]
                    [ div [ css [ displayFlex, justifyContent spaceBetween, alignItems center, marginBottom (rem 1) ] ]
                        [ h3 [ css [ margin zero, fontSize (rem 1.1) ] ] [ text (I18n.chooseEmoji lang) ]
                        , button
                            [ type_ "button"
                            , onClick CloseEmojiPicker
                            , css [ backgroundColor (hex "F1F5F9"), border zero, borderRadius (px 10), padding2 (rem 0.35) (rem 0.65), cursor pointer, fontWeight (int 700) ]
                            ]
                            [ text "✕" ]
                        ]
                    , div [ css [ displayFlex, flexDirection column, property "gap" "1rem" ] ]
                        (List.map (viewCategory lang) emojiCategories)
                    ]
                ]


viewCategory : Language -> ( String, List String ) -> Html FrontendMsg
viewCategory lang ( title, emojis ) =
    div []
        [ div [ css [ fontSize (rem 0.78), fontWeight (int 700), color (hex "64748B"), marginBottom (rem 0.4), textTransform uppercase ] ] [ text (I18n.emojiCategory lang title) ]
        , div
            [ css
                [ property "display" "grid"
                , property "grid-template-columns" "repeat(6, 1fr)"
                , property "gap" "0.4rem"
                ]
            ]
            (List.map viewEmojiCell emojis)
        ]


viewEmojiCell : String -> Html FrontendMsg
viewEmojiCell emojiStr =
    button
        [ type_ "button"
        , onClick (PickEmoji emojiStr)
        , css
            [ fontSize (rem 1.5)
            , padding (rem 0.45)
            , borderRadius (px 12)
            , border3 (px 1) solid (hex "E2E8F0")
            , backgroundColor (hex "F8FAFC")
            , cursor pointer
            ]
        ]
        [ text emojiStr ]
