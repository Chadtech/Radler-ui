module View exposing (view)

import Array
import Browser
import Colors
import Css exposing (..)
import Data.Tracker exposing (Tracker)
import Header
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style
import Tracker


-- VIEW --


view : Model -> Browser.Document Msg
view model =
    { title = "Radler"
    , body =
        [ Header.view model
            |> Html.map HeaderMsg
        , bodyContainer model
        ]
            |> List.map Html.toUnstyled
    }



-- BODY --


bodyContainer : Model -> Html Msg
bodyContainer model =
    Grid.row
        [ css [ flex (int 1) ] ]
        [ Grid.column
            [ css
                [ Style.card
                , Style.basicSpacing
                , overflow hidden
                ]
            ]
            [ body model ]
        ]


body : Model -> Html Msg
body model =
    div
        [ css
            [ Style.indent
            , width (pct 100)
            , Style.basicSpacing
            , backgroundColor Colors.background1
            , overflow auto
            ]
        ]
        [ Grid.container
            [ css
                [ display inlineFlex
                , Style.basicSpacing
                ]
            ]
            (viewTrackers model)
        ]



-- TRACKERS --


viewTrackers : Model -> List (Html Msg)
viewTrackers model =
    model.trackers
        |> Array.toIndexedList
        |> List.map (viewTracker model)


viewTracker : Model -> ( Int, ( Int, Tracker ) ) -> Html Msg
viewTracker model ( trackerIndex, rest ) =
    rest
        |> Tracker.view model
        |> Html.map (TrackerMsg trackerIndex)
