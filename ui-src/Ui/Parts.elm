module Ui.Parts exposing
    ( Msg
    , update
    , view
    )

import Colors
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style



-- TYPES --


type Msg
    = Noop



-- UPDATE --


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model



-- VIEW --


view : Model -> Html Msg
view model =
    Html.text "parts!!!!!"
