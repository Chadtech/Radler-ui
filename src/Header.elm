module Header
    exposing
        ( Msg
        , update
        , view
        )

import Css exposing (..)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Model exposing (Model)
import Return2 as R2
import Style


-- TYPES --


type Msg
    = None



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            model
                |> R2.withNoCmd



-- VIEW --


view : Model -> Html Msg
view model =
    Grid.row
        [ css
            [ Style.card
            , displayFlex
            , height (px 16)
            ]
        ]
        []
