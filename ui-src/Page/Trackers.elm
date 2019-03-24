module Page.Trackers exposing (view)

import Array
import Colors
import Css exposing (..)
import Data.Tracker exposing (Tracker)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style
import Ui.Tracker as Tracker


view : Model -> Html Msg
view model =
    Html.div
        [ Attrs.css
            [ Style.indent
            , width (pct 100)
            , Style.basicSpacing
            , backgroundColor Colors.background1
            , overflow auto
            ]
        ]
        [ Grid.container
            [ display inlineFlex
            , Style.basicSpacing
            ]
            (viewTrackers model)
        ]


viewTrackers : Model -> List (Html Msg)
viewTrackers model =
    let
        viewTracker : ( Int, Tracker ) -> Html Msg
        viewTracker ( trackerIndex, tracker ) =
            tracker
                |> Tracker.view model trackerIndex
                |> Html.map (TrackerMsg trackerIndex)
    in
    model.trackers
        |> Array.toIndexedList
        |> List.map viewTracker
