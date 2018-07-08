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
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy
import Model exposing (Model)
import Return2 as R2
import Row
import Style
import Util


-- TYPES --


type alias Payload =
    { sheet : Sheet
    , majorMark : Int
    , minorMark : Int
    , size : Style.Size
    , sheetDetails : Maybe (List ( Int, String ))
    }


type Msg
    = RowMsg Int Row.Msg
    | DetailsMsg Details.Msg
    | NameClicked



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
        (header payload)
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



-- HEADER --


header : Payload -> List (Html Msg)
header { sheet, size } =
    List.range 0 (Sheet.columnCount sheet - 1)
        |> List.map (columnNumbers size)
        |> (::) (sheetNameView sheet size)


columnNumbers : Style.Size -> Int -> Html Msg
columnNumbers size i =
    Grid.column
        []
        [ button
            [ css
                [ Style.basicButton size
                , width (px (Style.cellWidth size + 2))
                , minHeight fitContent
                ]
            ]
            [ Html.text (String.fromInt i)
            ]
        ]


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


sheetOptions : () -> Html Msg
sheetOptions () =
    div
        []
        []


sheetNameStyle : Style.Size -> Style
sheetNameStyle size =
    [ Style.basicButton size
    , width (px (Style.cellWidth size * 2 + 4))
    , hover [ color Colors.point1 ]
    , active [ Style.indent ]
    , cursor pointer
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
