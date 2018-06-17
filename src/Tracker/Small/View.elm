module Tracker.Small.View
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
                , alignSelf flexEnd
                ]
            ]


sheetNameView : Sheet -> Html Msg
sheetNameView sheet =
    button
        [ css
            [ Css.batch rowButtonStyle
            , flex2 (int 1) (int 1)
            , width (px 94)
            ]
        ]
        [ Html.text sheet.name ]


rowHeaderView : Int -> Html Msg
rowHeaderView columnIndex =
    button
        [ css
            [ Css.batch rowButtonStyle
            , flex2 (int 0) (int 1)
            , flexBasis (px 60)
            , width (px 60)
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
    [ displayFlex
    , alignSelf flexStart
    ]


rowDeleteView : Payload -> Int -> Html Msg
rowDeleteView payload index =
    button
        [ css rowButtonStyleClickable ]
        [ Html.text "x" ]


rowPlusView : Payload -> Int -> Html Msg
rowPlusView payload index =
    button
        [ css rowButtonStyleClickable ]
        [ Html.text "+v" ]


rowButtonStyleClickable : List Style
rowButtonStyleClickable =
    [ Css.batch rowButtonStyle
    , active [ Style.indent ]
    , hover [ color Colors.point1 ]
    ]


rowButtonStyle : List Style
rowButtonStyle =
    [ Style.outdent
    , Style.hftin
    , margin (px 1)
    , flex2 (int 0) (int 1)
    , flexBasis (px 30)
    , width (px 30)
    , height (px 16)
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]


rowNumberView : Payload -> Int -> Html Msg
rowNumberView payload index =
    button
        [ css rowButtonStyle ]
        [ Html.text (String.fromInt index) ]


cellView : Payload -> Int -> ( Int, String ) -> Html Msg
cellView payload rowIndex ( cellIndex, str ) =
    input
        [ css cellStyle
        , Attrs.value str
        , Attrs.spellcheck False
        ]
        []


cellStyle : List Style
cellStyle =
    [ Style.basicInput
    , Style.hftin
    , color Colors.point0
    , width (px cellWidth)
    , margin (px 1)
    , flex2 (int 0) (int 1)
    , Style.fontSmoothingNone
    ]


cellWidth : Float
cellWidth =
    60
