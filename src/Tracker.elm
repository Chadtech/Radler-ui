module Tracker
    exposing
        ( Msg(..)
        , view
        )

import Array exposing (Array)
import Css exposing (..)
import Data.Sheet as Sheet exposing (Sheet)
import Data.Tracker as Tracker
    exposing
        ( Tracker(..)
        )
import Html.Custom exposing (p)
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        )
import Html.Styled.Attributes exposing (css)
import Html.Styled.Lazy
import Model exposing (Model)
import Row
import Style


-- TYPES --


type Msg
    = RowMsg Int Row.Msg



-- VIEW --


view : Model -> ( Int, Tracker ) -> Html Msg
view model ( sheetIndex, tracker ) =
    case Array.get sheetIndex model.sheets of
        Just sheet ->
            Html.Styled.Lazy.lazy4
                fromPayload
                sheet
                model.majorMark
                model.minorMark
                tracker

        --case tracker of
        --Big ->
        --    { sheet = sheet
        --    , majorMark = model.majorMark
        --    , minorMark = model.minorMark
        --    , fontStyle = Css.batch [ Style.hfnss ]
        --    , cellWidth = 90
        --    , cellHeight = 26
        --    }
        --        |> fromPayload
        --Small ->
        --    { sheet = sheet
        --    , majorMark = model.majorMark
        --    , minorMark = model.minorMark
        --    , fontStyle = Css.batch [ Style.hftin ]
        --    , cellWidth = 60
        --    , cellHeight = 16
        --    }
        --        |> fromPayload
        Nothing ->
            notFoundView


fromPayload : Sheet -> Int -> Int -> Tracker -> Html Msg
fromPayload sheet majorMark minorMark tracker =
    Grid.container
        [ css
            [ Style.card
            , flexDirection Css.column
            , height (calc (vh 100) minus (px 68))
            , overflow hidden
            ]
        ]
        [ Grid.row
            [ css [ minHeight fitContent ] ]
            (header sheet tracker)
        , Grid.row
            []
            [ Grid.container
                [ css [ overflow auto ] ]
                (viewRows sheet majorMark minorMark tracker)
            ]
        ]


viewRows : Sheet -> Int -> Int -> Tracker -> List (Html Msg)
viewRows sheet majorMark minorMark tracker =
    sheet.rows
        |> Array.toIndexedList
        |> List.map (wrapRow majorMark minorMark tracker)


wrapRow : Int -> Int -> Tracker -> ( Int, Array String ) -> Html Msg
wrapRow majorMark minorMark tracker ( index, row ) =
    Html.Styled.Lazy.lazy5
        Row.view
        majorMark
        minorMark
        tracker
        index
        row
        |> Html.map (RowMsg index)



-- HEADER --


header : Sheet -> Tracker -> List (Html Msg)
header sheet tracker =
    List.range 0 (Sheet.columnCount sheet - 1)
        |> List.map (columnNumbers tracker)
        |> (::) (sheetNameView sheet tracker)


columnNumbers : Tracker -> Int -> Html Msg
columnNumbers tracker i =
    Grid.column
        []
        [ button
            [ css
                [ Row.buttonStyle tracker
                , width (px (Tracker.cellWidth tracker + 2))
                , minHeight fitContent
                ]
            ]
            [ Html.text (String.fromInt i)
            ]
        ]


sheetNameView : Sheet -> Tracker -> Html Msg
sheetNameView sheet tracker =
    Grid.column
        []
        [ button
            [ css
                [ Row.buttonStyle tracker
                , width
                    (px (Tracker.cellWidth tracker * 1.5 + 4))
                ]
            ]
            [ Html.text sheet.name ]
        ]



-- NOT FOUND VIEW --


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
