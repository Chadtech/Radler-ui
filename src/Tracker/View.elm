module Tracker.View
    exposing
        ( view
        )

import Array
import Css exposing (..)
import Data.Sheet exposing (Sheet)
import Html
import Html.Styled exposing (Attribute, Html, div, p)
import Html.Styled.Attributes exposing (css)
import Model exposing (Model, Tracker(..))
import Msg exposing (Msg(..))
import Style
import Tracker.Msg
import Tracker.Payload exposing (Payload)
import Tracker.View.Big as Big
import Tracker.View.Small as Small


view : Model -> ( Int, ( Int, Tracker ) ) -> Html.Html Msg
view model ( trackerIndex, ( sheetIndex, tracker ) ) =
    ( trackerIndex
    , ( Array.get sheetIndex model.sheets
      , tracker
      )
    )
        |> viewTracker model
        |> Html.Styled.map (TrackerMsg trackerIndex)
        |> Html.Styled.toUnstyled


viewTracker : Model -> ( Int, ( Maybe Sheet, Tracker ) ) -> Html Tracker.Msg.Msg
viewTracker model ( trackerIndex, ( maybeSheet, tracker ) ) =
    case maybeSheet of
        Just sheet ->
            case tracker of
                Big ->
                    makePayload model sheet
                        |> Big.view

                Small ->
                    makePayload model sheet
                        |> Small.view

        Nothing ->
            notFoundView


makePayload : Model -> Sheet -> Payload
makePayload model sheet =
    { sheet = sheet
    , majorMark = model.majorMark
    , minorMark = model.minorMark
    }


notFoundView : Html Tracker.Msg.Msg
notFoundView =
    div
        [ css [ Style.cardContainer ] ]
        [ p
            [ css
                [ Style.basicP
                , Style.hfnss
                , whiteSpace noWrap
                , margin (px 4)
                ]
            ]
            [ Html.Styled.text "Error : Sheet not found" ]
        ]
