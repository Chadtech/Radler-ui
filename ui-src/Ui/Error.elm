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
import Html.Styled.Events as Events
import Json.Decode as Decode
import Model exposing (Model)
import Style



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
    Grid.container
        []
        [ Grid.row
            [ marginBottom (px 5) ]
            [ Grid.column
                []
                [ Html.p
                    [ Attrs.css [ errorPStyle ] ]
                    [ Html.text (Error.toString error) ]
                ]
            ]
        , Grid.row
            []
            [ Grid.column
                [ justifyContent center ]
                [ ignoreButton ]
            ]
        ]


ignoreButton : Html Msg
ignoreButton =
    Html.button
        [ Attrs.css
            [ Style.basicButton Style.Big
            , width (px (Style.noteWidth Style.Big))
            , active [ Style.indent ]
            ]
        , Events.onClick IgnoreClicked
        ]
        [ Html.text "ignore" ]


{-| The view for when the app fails to initialize
-}
initializationErrorView : Decode.Error -> Html msg
initializationErrorView error =
    Grid.container
        [ marginTop (pct 50)
        , transform (translateY (pct -50))
        ]
        [ Grid.row
            []
            [ Grid.column
                [ Style.card
                , Style.bigSpacing
                ]
                [ Html.p
                    [ Attrs.css [ errorPStyle ] ]
                    [ Html.text (Decode.errorToString error) ]
                ]
            ]
        ]


errorPStyle : Style
errorPStyle =
    [ Style.hfnss
    , property "word-wrap" "break-word"
    , maxWidth (px 500)
    ]
        |> Css.batch
