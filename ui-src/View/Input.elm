module View.Input exposing
    ( Input
    , Model
    , Option
    , config
    , makeTallerBy
    , toHtml
    , withBackgroundColor
    , withHtmlAttr
    , withId
    , withSize
    , withWidth
    )

import Css exposing (..)
import Data.Size as Size exposing (Size)
import Data.Width exposing (Width)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Style
import Util.Css as CssUtil



-- TYPES --


type Input msg
    = Input (Model msg)


type Option msg
    = Size Size
    | HtmlAttr (Attribute msg)
    | Id String
    | BackgroundColor Color
    | TallerBy Float
    | Width Width


type alias Model msg =
    { value : String
    , onInput : String -> msg
    , options : List (Option msg)
    }


type alias Summary msg =
    { size : Size
    , attrs : List (Attribute msg)
    , id : Maybe String
    , backgroundColor : Maybe Color
    , extraHeight : Float
    , width : Maybe Width
    }



-- OPTIONS --


withWidth : Width -> Input msg -> Input msg
withWidth =
    addOption << Width


makeTallerBy : Float -> Input msg -> Input msg
makeTallerBy =
    addOption << TallerBy


withBackgroundColor : Color -> Input msg -> Input msg
withBackgroundColor =
    addOption << BackgroundColor


withId : String -> Input msg -> Input msg
withId =
    addOption << Id


withHtmlAttr : Attribute msg -> Input msg -> Input msg
withHtmlAttr =
    addOption << HtmlAttr


withSize : Size -> Input msg -> Input msg
withSize =
    addOption << Size


addOption : Option msg -> Input msg -> Input msg
addOption option (Input model) =
    Input { model | options = option :: model.options }



-- SUMMARY --


optionsToSummary : List (Option msg) -> Summary msg
optionsToSummary =
    let
        modifySummary : Option msg -> Summary msg -> Summary msg
        modifySummary option summary =
            case option of
                Size size ->
                    { summary | size = size }

                HtmlAttr attr ->
                    { summary | attrs = attr :: summary.attrs }

                Id id ->
                    { summary | id = Just id }

                BackgroundColor bgColor ->
                    { summary | backgroundColor = Just bgColor }

                TallerBy extraHeight ->
                    { summary | extraHeight = summary.extraHeight + extraHeight }

                Width width_ ->
                    { summary | width = Just width_ }
    in
    List.foldr modifySummary
        { size = Size.Big
        , attrs = []
        , id = Nothing
        , backgroundColor = Nothing
        , extraHeight = 0
        , width = Nothing
        }


config : (String -> msg) -> String -> Input msg
config msgCtor value =
    Input
        { value = value
        , onInput = msgCtor
        , options = []
        }


toHtml : Input msg -> Html msg
toHtml (Input { value, onInput, options }) =
    let
        summary : Summary msg
        summary =
            optionsToSummary options

        maybeAttrs : List (Attribute msg)
        maybeAttrs =
            [ Maybe.map Attrs.id summary.id ]
                |> List.filterMap identity

        attrs : List (Attribute msg)
        attrs =
            [ Attrs.css
                [ Style.font summary.size
                , height (px (Size.toUnitHeight summary.size + summary.extraHeight))
                , width summary
                , CssUtil.styleMaybe backgroundColor summary.backgroundColor
                ]
            , Attrs.value value
            , Attrs.spellcheck False
            , Events.onInput onInput
            ]
                ++ summary.attrs
                ++ maybeAttrs
    in
    Html.input attrs []


width : Summary msg -> Style
width summary =
    case summary.width of
        Nothing ->
            Css.width
                (px (Size.toUnitWidth summary.size))

        Just width_ ->
            Style.width summary.size width_
