module Tracker.View
    exposing
        ( view
        )

import Array
import Css exposing (..)
import Data.Sheet exposing (Sheet)
import Data.Tracker
    exposing
        ( Payload
        , Tracker(..)
        )
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , div
        , p
        )
import Html.Styled.Attributes exposing (css)
import Model exposing (Model)
import Style
import Tracker.Msg exposing (Msg)
import Tracker.View.Big as Big
import Tracker.View.Small as Small


view : Model -> ( Int, ( Int, Tracker ) ) -> Html Msg
view model ( trackerIndex, ( sheetIndex, tracker ) ) =
    ( trackerIndex
    , ( Array.get sheetIndex model.sheets
      , tracker
      )
    )
        |> viewBody model


viewBody : Model -> ( Int, ( Maybe Sheet, Tracker ) ) -> Html Msg
viewBody model ( trackerIndex, ( maybeSheet, tracker ) ) =
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


notFoundView : Html Msg
notFoundView =
    div
        [ css [ Style.card ] ]
        [ p
            [ css
                [ Style.basicP
                , Style.hfnss
                , whiteSpace noWrap
                , margin (px 4)
                ]
            ]
            [ Html.text "Error : Sheet not found" ]
        ]
