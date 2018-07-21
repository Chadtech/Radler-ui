module Package
    exposing
        ( Msg
        , update
        , view
        )

import Colors
import Css exposing (..)
import Html.Styled as Html exposing (Html, div, textarea)
import Html.Styled.Events exposing (onInput)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
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
            { model | package = str }



-- VIEW --


view : Model -> Html Msg
view model =
    textarea
        [ css
            [ Style.basicInput
            , color Colors.point0
            , Style.fontSmoothingNone
            , Style.basicSpacing
            , width (pct 100)
            , Style.hfnss
            ]
        , Attrs.spellcheck False
        , onInput PackageUpdated
        ]
        []
