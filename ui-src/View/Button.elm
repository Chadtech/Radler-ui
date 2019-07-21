module View.Button exposing
    ( Button
    , Option
    , Width
    , config
    , doubleWidth
    , fullWidth
    , halfWidth
    , indent
    , isDisabled
    , makeTallerBy
    , singleWidth
    , toHtml
    , withSize
    , withWidth
    )

import Colors
import Css exposing (Style, active, backgroundColor, height, pct, px)
import Data.Size as Size exposing (Size)
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


type Width
    = HalfWidth
    | SingleWidth
    | DoubleWidth
    | FullWidth


type alias Summary =
    { size : Size
    , width : Maybe Width
    , extraHeight : Float
    , indent : Maybe Bool
    , disabled : Bool
    }



-- VALUES --


halfWidth : Width
halfWidth =
    HalfWidth


singleWidth : Width
singleWidth =
    SingleWidth


doubleWidth : Width
doubleWidth =
    DoubleWidth


fullWidth : Width
fullWidth =
    FullWidth



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
            [ Style.buttonStyle summary.size
            , buttonWidth summary
            , indentStyle summary.indent
            , disabledStyle summary.disabled
            , height (px <| Style.noteHeight summary.size + summary.extraHeight)
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
            CssUtil.noStyle

        Just True ->
            Style.indent

        Just False ->
            Style.outdent


buttonWidth : Summary -> Style
buttonWidth summary =
    case summary.width of
        Nothing ->
            CssUtil.noStyle

        Just HalfWidth ->
            Style.halfWidth summary.size

        Just SingleWidth ->
            Style.singleWidth summary.size

        Just DoubleWidth ->
            Style.doubleWidth summary.size

        Just FullWidth ->
            Css.width (pct 100)
