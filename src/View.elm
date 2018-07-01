module View exposing (view)

import Array
import Browser
import Css exposing (..)
import Data.Tracker exposing (Tracker)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style
import Tracker.View as Tracker


-- VIEW --


view : Model -> Browser.Document Msg
view model =
    { title = "Radler"
    , body =
        [ header
        , bodyView model
        ]
            |> List.map Html.toUnstyled
    }


header : Html Msg
header =
    Grid.row
        []
        [ Grid.column
            []
            [ div
                [ css
                    [ Style.card
                    , displayFlex
                    , height (px 16)
                    ]
                ]
                []
            ]
        ]


bodyView : Model -> Html Msg
bodyView model =
    Grid.row
        []
        [ Grid.column
            [ css [ overflow auto ] ]
            [ Grid.container
                [ css [ display inlineFlex ] ]
                (viewTrackers model)
            ]
        ]


viewTrackers : Model -> List (Html Msg)
viewTrackers model =
    model.trackers
        |> Array.toIndexedList
        |> List.map (viewTracker model)


viewTracker : Model -> ( Int, ( Int, Tracker ) ) -> Html Msg
viewTracker model ( index, rest ) =
    ( index, rest )
        |> Tracker.view model
        |> Html.map (TrackerMsg index)
