module View.TextArea exposing
    ( config
    , toHtml
    , withStyles
    )

import Colors
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Style



-- TYPES ---


type alias Model msg =
    { value : String
    , onUpdate : String -> msg
    , options : List Option
    }


type Option
    = Style (List Style)


type TextArea msg
    = TextArea (Model msg)


type alias Summary =
    { extraStyles : List Style }



-- OPTIONS --


withStyles : List Style -> TextArea msg -> TextArea msg
withStyles =
    addOption << Style


addOption : Option -> TextArea msg -> TextArea msg
addOption option (TextArea model) =
    TextArea { model | options = option :: model.options }



-- SUMMARY --


optionsToSummary : List Option -> Summary
optionsToSummary =
    let
        modifySummary : Option -> Summary -> Summary
        modifySummary option summary =
            case option of
                Style extraStyles ->
                    { summary
                        | extraStyles =
                            extraStyles ++ summary.extraStyles
                    }
    in
    List.foldr modifySummary
        { extraStyles = [] }



-- TEXT AREA --


config : (String -> msg) -> String -> TextArea msg
config onUpdate value =
    TextArea
        { onUpdate = onUpdate
        , value = value
        , options = []
        }


toHtml : TextArea msg -> Html msg
toHtml (TextArea model) =
    let
        summary : Summary
        summary =
            optionsToSummary model.options
    in
    Html.textarea
        [ Attrs.css
            [ color Colors.point0
            , Style.fontSmoothingNone
            , width (pct 100)
            , Style.hfnss
            , Css.batch summary.extraStyles
            ]
        , Attrs.spellcheck False
        , Attrs.value model.value
        , Events.onInput model.onUpdate
        ]
        []
