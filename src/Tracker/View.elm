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
import Tracker.Big.View as Big
import Tracker.Msg as Tracker
import Tracker.Small.View as Small


view : Model -> ( Int, ( Int, Tracker ) ) -> Html.Html Msg
view model ( trackerIndex, ( sheetIndex, tracker ) ) =
    ( trackerIndex
    , ( Array.get sheetIndex model.sheets
      , tracker
      )
    )
        |> viewTracker
        |> Html.Styled.map (TrackerMsg trackerIndex)
        |> Html.Styled.toUnstyled


viewTracker : ( Int, ( Maybe Sheet, Tracker ) ) -> Html Tracker.Msg
viewTracker ( trackerIndex, ( maybeSheet, tracker ) ) =
    case maybeSheet of
        Just sheet ->
            case tracker of
                Big ->
                    { sheet = sheet }
                        |> Big.view

                Small ->
                    { sheet = sheet }
                        |> Small.view

        Nothing ->
            notFoundView


notFoundView : Html Tracker.Msg
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
