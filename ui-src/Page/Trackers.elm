module Page.Trackers exposing (view)

import Colors
import Css exposing (..)
import Data.Index as Index exposing (Index)
import Data.Part exposing (Part)
import Data.Tracker exposing (Tracker)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style
import Ui.Tracker as Tracker
import View.Card as Card
import View.Text as Text


view : Model -> List (Grid.Column Msg)
view model =
    [ Grid.column
        [ Style.indent
        , Style.fullWidth
        , Style.basicSpacing
        , backgroundColor Colors.background1
        , overflowX auto
        , overflowY hidden
        , display inlineFlex
        ]
        (viewTrackers model)
    ]


viewTrackers : Model -> List (Html Msg)
viewTrackers model =
    let
        partNames : List ( Index Part, String )
        partNames =
            Model.indexedPartNames model

        viewTracker : ( Index Tracker, Tracker ) -> Html Msg
        viewTracker ( trackerIndex, tracker ) =
            case Model.getPart tracker.partIndex model of
                Just part ->
                    Tracker.view
                        { trackerIndex = trackerIndex
                        , tracker = tracker
                        , part = part
                        , partNames = partNames
                        }
                        |> Html.map (TrackerMsg trackerIndex)

                Nothing ->
                    notFoundView
    in
    model.trackers
        |> Index.toEntries
        |> List.map viewTracker


notFoundView : Html Msg
notFoundView =
    Card.config []
        [ Text.withStyles
            [ whiteSpace noWrap
            , margin (px 4)
            ]
            "Error : Part not found"
        ]
