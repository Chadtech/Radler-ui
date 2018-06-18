module Tracker.View.Big
    exposing
        ( view
        )

import Array exposing (Array)
import Colors
import Css exposing (..)
import Data.Sheet as Sheet exposing (Sheet)
import Html.Custom exposing (p)
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , input
        )
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Model exposing (Model)
import Style
import Tracker.Msg exposing (Msg)
import Tracker.Payload exposing (Payload)


-- VIEW --


view : Payload -> Html Msg
view payload =
    div
        [ css [ Style.cardContainer ] ]
        (viewBody payload)


viewBody : Payload -> List (Html Msg)
viewBody payload =
    payload.sheet.rows
        |> Array.toIndexedList
        |> List.map (rowView payload)
        |> (::) (rowHeadersView payload.sheet)


rowHeadersView : Sheet -> Html Msg
rowHeadersView sheet =
    List.range 0 (Sheet.columnCount sheet - 1)
        |> List.map rowHeaderView
        |> (::) (sheetNameView sheet)
        |> div
            [ css
                [ Css.batch rowStyle
                , justifyContent flexEnd
                ]
            ]


sheetNameView : Sheet -> Html Msg
sheetNameView sheet =
    button
        [ css
            [ Css.batch rowButtonStyle
            , flex2 (int 1) (int 1)
            , width (px 139)
            ]
        ]
        [ Html.text sheet.name ]


rowHeaderView : Int -> Html Msg
rowHeaderView columnIndex =
    button
        [ css
            [ Css.batch rowButtonStyle
            , flex2 (int 0) (int 1)
            , flexBasis (px cellWidth)
            ]
        ]
        [ Html.text (String.fromInt columnIndex)
        ]


rowView : Payload -> ( Int, Array String ) -> Html Msg
rowView payload ( index, row ) =
    row
        |> Array.toIndexedList
        |> List.map (cellView payload index)
        |> (::) (rowNumberView payload index)
        |> (::) (rowPlusView payload index)
        |> (::) (rowDeleteView payload index)
        |> div [ css rowStyle ]


rowStyle : List Style
rowStyle =
    [ displayFlex ]


rowDeleteView : Payload -> Int -> Html Msg
rowDeleteView payload index =
    button
        [ css rowButtonStyle ]
        [ Html.text "x" ]


rowPlusView : Payload -> Int -> Html Msg
rowPlusView payload index =
    button
        [ css rowButtonStyle ]
        [ Html.text "+v" ]


rowButtonStyle : List Style
rowButtonStyle =
    [ Style.outdent
    , Style.hfnss
    , active [ Style.indent ]
    , hover [ color Colors.point1 ]
    , margin (px 1)
    , flex2 (int 0) (int 1)
    , flexBasis (px 45)
    , width (px 45)
    , height (px 24)
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]


rowNumberView : Payload -> Int -> Html Msg
rowNumberView payload index =
    p
        [ css rowNumberStyle ]
        [ Html.text (String.fromInt index) ]


rowNumberStyle : List Style
rowNumberStyle =
    [ Style.outdent
    , Style.hfnss
    , margin (px 1)
    , textAlign center
    , height (px 20)
    , flex2 (int 1) (int 1)
    , width (px 45)
    , lineHeight (px 24)
    ]


cellView : Payload -> Int -> ( Int, String ) -> Html Msg
cellView payload rowIndex ( cellIndex, str ) =
    input
        [ css (cellStyle payload rowIndex)
        , Attrs.value str
        , Attrs.spellcheck False
        ]
        []


cellStyle : Payload -> Int -> List Style
cellStyle payload rowIndex =
    [ Style.indent
    , outline none
    , determineCellBgColor payload rowIndex
        |> backgroundColor
    , Style.hfnss
    , color Colors.point0
    , width (px cellWidth)
    , margin (px 1)
    , flex2 (int 0) (int 1)
    , Style.fontSmoothingNone
    , height (px 24)
    ]


determineCellBgColor : Payload -> Int -> Color
determineCellBgColor { majorMark, minorMark } rowIndex =
    if remainderBy majorMark rowIndex == 0 then
        Colors.background4
    else if remainderBy minorMark rowIndex == 0 then
        Colors.background3
    else
        Colors.background2


cellWidth : Float
cellWidth =
    80
