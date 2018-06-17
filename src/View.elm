module View exposing (view)

import Browser
import Colors
import Css exposing (..)
import Html.Custom exposing (input, p)
import Html.Styled as Html exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css, placeholder, spellcheck, value)
import Html.Styled.Events exposing (onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))


-- STYLES --


big : Attribute Msg
big =
    [ fontSize (em 4) ]
        |> css


tin : Attribute Msg
tin =
    [ fontFamilies [ "HFTIN" ] ]
        |> css


nss : Attribute Msg
nss =
    [ fontFamilies [ "HFNSS" ] ]
        |> css



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
    [ p [ tin ] [ Html.text "abcdefg" ]
    , p [ nss ] [ Html.text "abcedfg" ]
    ]
