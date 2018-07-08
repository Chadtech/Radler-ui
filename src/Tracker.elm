module Tracker
    exposing
        ( Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Cell
import Colors
import Css exposing (..)
import Data.Sheet as Sheet exposing (Sheet)
import Data.Tracker as Tracker exposing (Tracker)
import Details
import Html.Buttons as Buttons
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
import Html.Styled.Events exposing (onClick, onMouseLeave)
import Html.Styled.Lazy
import Model exposing (Model)
import Return2 as R2
import Row
import Set exposing (Set)
import Style
import Util


-- TYPES --


type alias Payload =
    { sheet : Sheet
    , majorMark : Int
    , minorMark : Int
    , size : Style.Size
    , sheetDetails : Maybe (List ( Int, String ))
    , toggledColumns : Set Int
    }


type Msg
    = RowMsg Int Row.Msg
    | DetailsMsg Details.Msg
    | NameClicked
    | DeleteTrackerClicked
    | AddRowBelowClicked
    | ColumnNumberClicked Int
    | ColumnNumberExited Int
    | DeleteColumnClicked Int
    | AddColumnClicked Int



-- UPDATE --


update : Int -> Int -> Msg -> Model -> Model
update ti si msg model =
    case msg of
        NameClicked ->
            Model.mapTracker
                ti
                Tracker.openDetails
                model

        DetailsMsg (Details.NameFieldUpdated str) ->
            Model.mapSheet
                si
                (Sheet.setName str)
                model

        DetailsMsg (Details.SheetClicked index) ->
            Model.mapTracker
                ti
                (Tracker.setSheetIndex index)
                model

        DetailsMsg Details.BackClicked ->
            Model.mapTracker
                ti
                Tracker.closeDetails
                model

        RowMsg ri subMsg ->
            Row.update si ri subMsg model

        DeleteTrackerClicked ->
            Model.removeTracker ti model

        AddRowBelowClicked ->
            Model.mapSheet
                si
                (Sheet.addRow -1)
                model

        ColumnNumberClicked index ->
            Model.mapTracker
                ti
                (Tracker.addToggledColumn index)
                model

        ColumnNumberExited index ->
            Model.mapTracker
                ti
                (Tracker.removeToggledColumn index)
                model

        AddColumnClicked index ->
            Model.mapSheet
                si
                (Sheet.addColumn index)
                model

        DeleteColumnClicked index ->
            model
                |> Model.mapSheet si (Sheet.removeColumn index)
                |> Model.mapTracker ti Tracker.clearToggledColumns



-- VIEW --


view : Model -> Tracker -> Html Msg
view model tracker =
    case Array.get tracker.sheetIndex model.sheets of
        Just sheet ->
            { sheet = sheet
            , majorMark = model.majorMark
            , minorMark = model.minorMark
            , size = tracker.size
            , sheetDetails =
                if tracker.sheetDetails then
                    model.sheets
                        |> Array.toIndexedList
                        |> List.map (Tuple.mapSecond .name)
                        |> Just
                else
                    Nothing
            , toggledColumns = tracker.toggledColumns
            }
                |> fromPayload

        Nothing ->
            notFoundView


fromPayload : Payload -> Html Msg
fromPayload payload =
    Grid.container
        [ Style.card
        , flexDirection Css.column
        , height (calc (vh 100) minus (px 78))
        , overflow hidden
        , position relative
        ]
        (contentView payload)


contentView : Payload -> List (Html Msg)
contentView payload =
    [ detailsContainerView payload
    , Grid.row
        [ minHeight fitContent ]
        (trackerOptions payload)
    , Grid.row
        [ minHeight fitContent
        , marginBottom (px 1)
        ]
        (columnOptions payload)
    , Grid.row
        [ minHeight fitContent
        , marginBottom (px 1)
        ]
        (columnNumbers payload)
    , Grid.row
        []
        [ Html.Styled.Lazy.lazy4
            rowsView
            payload.sheet
            payload.majorMark
            payload.minorMark
            payload.size
        ]
    ]


detailsContainerView : Payload -> Html Msg
detailsContainerView payload =
    case payload.sheetDetails of
        Just sheetNames ->
            { sheetNameField = payload.sheet.name
            , sheets = sheetNames
            }
                |> Details.view
                |> List.singleton
                |> div [ css [ detailsContainerStyle ] ]
                |> Html.map DetailsMsg

        Nothing ->
            Html.text ""


detailsContainerStyle : Style
detailsContainerStyle =
    [ backgroundColor (rgba 0 0 0 0.5)
    , width (pct 100)
    , height (pct 100)
    , position absolute
    , zIndex (int 2)
    ]
        |> Css.batch


rowsView : Sheet -> Int -> Int -> Style.Size -> Html Msg
rowsView sheet majorMark minorMark size =
    Grid.container
        [ overflow auto ]
        (rowsContentView sheet majorMark minorMark size)


rowsContentView : Sheet -> Int -> Int -> Style.Size -> List (Html Msg)
rowsContentView sheet majorMark minorMark size =
    sheet.rows
        |> Array.toIndexedList
        |> List.map (wrapRow majorMark minorMark size)


wrapRow : Int -> Int -> Style.Size -> ( Int, Array String ) -> Html Msg
wrapRow majorMark minorMark size ( index, row ) =
    Html.Styled.Lazy.lazy5
        Row.view
        majorMark
        minorMark
        size
        index
        row
        |> Html.map (RowMsg index)



-- TRACKER OPTIONS --


trackerOptions : Payload -> List (Html Msg)
trackerOptions payload =
    [ Grid.column
        [ flex none
        , flexBasis (px (Style.cellWidth payload.size + 4))
        , position relative
        ]
        [ button
            [ css
                [ Style.basicButton payload.size
                , width (px (Style.cellWidth payload.size + 2))
                , height (px ((Style.cellHeight payload.size * 2) + 2))
                , position absolute
                , top (px 0)
                , left (px 0)
                , hover [ color Colors.point1 ]
                , active [ Style.indent ]
                ]
            , onClick DeleteTrackerClicked
            ]
            [ Html.text "x" ]
        ]
    , Grid.column
        []
        [ sheetNameView payload.sheet payload.size ]
    ]



-- COLUMN OPTIONS --


columnOptions : Payload -> List (Html Msg)
columnOptions { sheet, toggledColumns, size } =
    List.range 0 (Sheet.columnCount sheet - 1)
        |> List.map (columnOption toggledColumns size)
        |> (::) (addColumnZero size)


columnOption : Set Int -> Style.Size -> Int -> Html Msg
columnOption toggledColumns size i =
    [ div
        [ css
            [ position absolute
            , top (px 0)
            , left (px 0)
            , width (px (Style.cellWidth size + 4))
            , minHeight fitContent
            ]
        ]
        [ button
            [ css
                [ Style.basicButton size
                , width (px (Style.cellWidth size / 2 - 1))
                , minHeight fitContent
                , active [ Style.indent ]
                , cursor pointer
                , hover [ color Colors.point1 ]
                ]
            , onClick (DeleteColumnClicked i)
            ]
            [ Html.text "x"
            ]
        , button
            [ css
                [ Style.basicButton size
                , width (px (Style.cellWidth size / 2 - 1))
                , minHeight fitContent
                , active [ Style.indent ]
                , cursor pointer
                , hover [ color Colors.point1 ]
                , marginRight (px 0)
                ]
            , onClick (AddColumnClicked i)
            ]
            [ Html.text "+>"
            ]
        ]
    ]
        |> Grid.column [ position relative ]


addColumnZero : Style.Size -> Html Msg
addColumnZero size =
    Grid.column
        []
        [ button
            [ css
                [ Style.basicButton size
                , width (px (Style.cellWidth size))
                , minHeight fitContent
                , active [ Style.indent ]
                , marginLeft (px (Style.cellWidth size + 5))
                ]
            , onClick (AddColumnClicked -1)
            ]
            [ Html.text "+>"
            ]
        ]



-- COLUMN NUMBERS --


columnNumbers : Payload -> List (Html Msg)
columnNumbers { sheet, toggledColumns, size } =
    List.range 0 (Sheet.columnCount sheet - 1)
        |> List.map (columnNumber toggledColumns size)
        |> (::) (addRowButton size)


addRowButton : Style.Size -> Html Msg
addRowButton size =
    Buttons.plus
        AddRowBelowClicked
        [ margin (px 0)
        , marginRight (px (Style.cellWidth size + 4))
        , marginLeft (px ((Style.cellWidth size / 2) + 3))
        ]
        size


columnNumber : Set Int -> Style.Size -> Int -> Html Msg
columnNumber toggledColumns size i =
    [ button
        [ css
            [ Style.basicButton size
            , width (px (Style.cellWidth size))
            , minHeight fitContent
            , position absolute
            , margin (px 0)
            , zIndex (int 1)
            ]
        ]
        [ Html.text (String.fromInt i) ]
    ]
        |> Grid.column [ position relative ]


sheetNameView : Sheet -> Style.Size -> Html Msg
sheetNameView sheet size =
    Grid.column
        []
        [ button
            [ css [ sheetNameStyle size ]
            , onClick NameClicked
            ]
            [ Html.text sheet.name ]
        ]


sheetNameStyle : Style.Size -> Style
sheetNameStyle size =
    [ Style.basicButton size
    , width (pct 100)
    , hover [ color Colors.point1 ]
    , active [ Style.indent ]
    ]
        |> Css.batch



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
