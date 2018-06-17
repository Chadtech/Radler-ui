module View exposing (view)

import Big
import Browser
import Colors
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css, placeholder, spellcheck, value)
import Html.Styled.Events exposing (onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Small


-- VIEW --


view : Model -> Browser.Page Msg
view model =
    { title = "Radler"
    , body =
        viewBody model
            |> List.map Html.toUnstyled
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ Small.view model
    , Big.view model
    ]
