module View.Button exposing
    ( Button
    , Option
    , config
    , indent
    , isDisabled
    , makeTallerBy
    , toHtml
    , withSize
    , withWidth
    )

import Colors
import Css exposing (..)
import Data.Size as Size exposing (Size)
import Data.Width as Width exposing (Width)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Style
import Util.Css as CssUtil



-- TYPES --


type Button msg
    = Button (Model msg)


type alias Model msg =
    { onClick : msg
    , label : String
    , options : List Option
    }


type Option
    = Size Size
    | Width Width
    | ExtraHeight Float
    | Indent Bool
    | Disabled Bool


type alias Summary =
    { size : Size
    , width : Maybe Width
    , extraHeight : Float
    , indent : Maybe Bool
    , disabled : Bool
    }



-- OPTIONS --


isDisabled : Bool -> Button msg -> Button msg
isDisabled =
    addOption << Disabled


withSize : Size -> Button msg -> Button msg
withSize =
    addOption << Size


makeTallerBy : Float -> Button msg -> Button msg
makeTallerBy =
    addOption << ExtraHeight


withWidth : Width -> Button msg -> Button msg
withWidth =
    addOption << Width


addOption : Option -> Button msg -> Button msg
addOption option (Button model) =
    Button { model | options = option :: model.options }


indent : Bool -> Button msg -> Button msg
indent =
    addOption << Indent



-- SUMMARY --


optionsToSummary : List Option -> Summary
optionsToSummary =
    let
        modifySummary : Option -> Summary -> Summary
        modifySummary option summary =
            case option of
                Size size ->
                    { summary | size = size }

                Width width ->
                    { summary | width = Just width }

                ExtraHeight extraheight ->
                    { summary | extraHeight = summary.extraHeight + extraheight }

                Indent indent_ ->
                    { summary | indent = Just indent_ }

                Disabled disabled ->
                    { summary | disabled = disabled }
    in
    List.foldr modifySummary
        { size = Size.big
        , width = Nothing
        , extraHeight = 0
        , indent = Nothing
        , disabled = False
        }



-- BUTTON --


config : msg -> String -> Button msg
config onClick label =
    Button
        { onClick = onClick
        , label = label
        , options = []
        }


toHtml : Button msg -> Html msg
toHtml (Button { onClick, label, options }) =
    let
        summary : Summary
        summary =
            optionsToSummary options
    in
    Html.button
        [ Attrs.css
            [ indentStyle summary.indent
            , Style.font summary.size
            , height (px <| Size.toUnitHeight summary.size + summary.extraHeight)
            , backgroundColor Colors.ignorable2
            , color Colors.point0
            , Style.fontSmoothingNone
            , padding (px 0)
            , outline none
            , active [ Style.indent ]
            , hover [ color Colors.point1 ]
            , cursor pointer
            , buttonWidth summary
            , disabledStyle summary.disabled
            ]
        , Events.onClick onClick
        ]
        [ Html.text label ]


disabledStyle : Bool -> Style
disabledStyle disabled =
    if disabled then
        [ backgroundColor Colors.ignorable3
        , active [ Style.outdent ]
        ]
            |> Css.batch

    else
        CssUtil.noStyle


indentStyle : Maybe Bool -> Style
indentStyle maybeIndent =
    case maybeIndent of
        Nothing ->
            Style.outdent

        Just True ->
            Style.indent

        Just False ->
            Style.outdent


buttonWidth : Summary -> Style
buttonWidth summary =
    Style.width
        summary.size
        (Maybe.withDefault Width.Single summary.width)
