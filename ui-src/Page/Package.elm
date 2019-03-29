module Page.Package exposing
    ( Msg
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Package as Package
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style
import Util.Css as CssUtil



-- TYPES --


type Msg
    = PackageUpdated String



-- UPDATE --


update : Msg -> Model -> Model
update msg =
    case msg of
        PackageUpdated str ->
            str
                |> Package.setJsonStrField
                |> Model.mapPackage



-- VIEW --


view : Model -> Html Msg
view model =
    Html.textarea
        [ Attrs.css
            [ color Colors.point0
            , Style.fontSmoothingNone
            , Style.basicSpacing
            , width (pct 100)
            , Style.hfnss
            , maybeInvalidBorders model
            , maybeInvalidBackground model
            ]
        , Attrs.spellcheck False
        , Attrs.value model.package.jsonStrField
        , Events.onInput PackageUpdated
        ]
        []


maybeInvalidBackground : Model -> Style
maybeInvalidBackground { package } =
    backgroundColor Colors.lowWarning
        |> List.singleton
        |> Css.batch
        |> CssUtil.styleIf (not package.validJson)


maybeInvalidBorders : Model -> Style
maybeInvalidBorders { package } =
    border3 (px 2) solid Colors.critical
        |> List.singleton
        |> Css.batch
        |> CssUtil.styleIf (not package.validJson)
