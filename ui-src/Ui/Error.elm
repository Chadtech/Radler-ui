module Ui.Error exposing
    ( Msg
    , initializationErrorView
    , modalView
    , update
    )

import Css exposing (..)
import Data.Error as Error exposing (Error(..))
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Json.Decode as Decode
import Model exposing (Model)
import Style
import View.Button as Button



-- TYPES --


type Msg
    = IgnoreClicked



-- UPDATE --


update : Msg -> Model -> Model
update msg =
    case msg of
        IgnoreClicked ->
            Model.clearModal



-- VIEW --


modalView : Error -> Html Msg
modalView error =
    Grid.box
        []
        [ Grid.row
            [ marginBottom (px 5) ]
            [ Grid.column
                []
                [ text (Error.toString error) ]
            ]
        , Grid.row
            []
            [ Grid.column
                [ justifyContent center ]
                [ Button.config IgnoreClicked "ignore"
                    |> Button.withWidth Button.singleWidth
                    |> Button.toHtml
                ]
            ]
        ]


{-| The view for when the app fails to initialize
-}
initializationErrorView : Decode.Error -> Html msg
initializationErrorView error =
    Grid.box
        [ height (vh 100)
        , position relative
        ]
        [ Grid.row
            [ height (pct 100) ]
            [ Grid.column
                [ Style.card
                , Style.bigSpacing
                , justifyContent center
                , overflow auto
                ]
                [ text (Decode.errorToString error) ]
            ]
        ]


text : String -> Html msg
text str =
    Html.p
        [ Attrs.css
            [ Style.hfnss
            , property "word-wrap" "break-word"
            , maxWidth (px 500)
            ]
        ]
        [ Html.text str ]
