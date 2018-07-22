module Package
    exposing
        ( Msg
        , update
        , view
        )

import Colors
import Css exposing (..)
import Data.Package as Package
import Html.Styled as Html exposing (Html, div, textarea)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onInput)
import Model exposing (Model)
import Style


-- TYPES --


type Msg
    = PackageUpdated String



-- UPDATE --


update : Msg -> Model -> Model
update msg model =
    case msg of
        PackageUpdated str ->
            Package.setJsonStrField str
                |> Model.mapPackage
                |> Model.apply model



-- VIEW --


view : Model -> Html Msg
view model =
    textarea
        [ Attrs.css
            [ Style.basicInput
            , color Colors.point0
            , Style.fontSmoothingNone
            , Style.basicSpacing
            , width (pct 100)
            , Style.hfnss
            ]
        , Attrs.spellcheck False
        , Attrs.value model.package.jsonStrField
        , onInput PackageUpdated
        ]
        []
