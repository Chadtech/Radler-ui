module View.Checkbox exposing
    ( Checkbox
    , Option
    , checkbox
    , toHtml
    )

-- TYPES --

import Css exposing (cursor, height, paddingLeft, pointer, px, width)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events


type Checkbox msg
    = Checkbox (Model msg)


type alias Model msg =
    { onClick : msg
    , checked : Bool
    , options : List Option
    }


type Option
    = Option


type alias Summary =
    {}



-- OPTIONS --


addOption : Option -> Checkbox msg -> Checkbox msg
addOption option (Checkbox model) =
    Checkbox { model | options = option :: model.options }



-- SUMMARY --


optionsToSummary : List Option -> Summary
optionsToSummary =
    let
        modifySummary : Option -> Summary -> Summary
        modifySummary option summary =
            case option of
                Option ->
                    summary
    in
    List.foldr modifySummary {}



-- CHECKBOX --


checkbox : msg -> Bool -> Checkbox msg
checkbox onClick checked =
    Checkbox { onClick = onClick, checked = checked, options = [] }


toHtml : Checkbox msg -> Html msg
toHtml (Checkbox { onClick, checked, options }) =
    let
        summary : Summary
        summary =
            optionsToSummary options
    in
    Html.input
        [ Events.onClick onClick
        , Attrs.value <| inputValue checked
        , Attrs.spellcheck False
        , Attrs.readonly True
        , Attrs.css
            [ height (px 30)
            , width (px 30)
            , cursor pointer
            , paddingLeft (px 7)
            ]
        ]
        []


inputValue : Bool -> String
inputValue checked =
    if checked then
        "x"

    else
        ""
