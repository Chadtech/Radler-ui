module Ui.Error exposing
    ( Msg
    , initializationErrorView
    , modalView
    , update
    )

import Css exposing (..)
import Data.Error as Error exposing (Error(..))
import Data.Width as Width
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Json.Decode as Decode
import Model exposing (Model)
import Style
import View.Button as Button
import View.Card as Card
import View.Text as Text



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
                    |> Button.withWidth Width.single
                    |> Button.toHtml
                ]
            ]
        ]


{-| The view for when the app fails to initialize
-}
initializationErrorView : Decode.Error -> Html msg
initializationErrorView error =
    Grid.row
        [ justifyContent center
        , height (pct 100)
        ]
        [ Grid.column
            [ Grid.columnShrink
            , flexDirection Css.column
            , justifyContent center
            , height (pct 100)
            ]
            [ Grid.box
                [ height (px 500)
                ]
                [ Card.config
                    [ Style.bigSpacing
                    , justifyContent center
                    , overflow auto
                    ]
                    [ text (Decode.errorToString error) ]
                ]
            ]
        ]


text : String -> Html msg
text =
    Text.withStyles
        [ property "word-wrap" "break-word"
        , maxWidth (px 500)
        ]
