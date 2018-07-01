module Tracker.View.Small.Cell
    exposing
        ( view
        , width
        )

import Colors
import Css exposing (..)
import Data.Tracker exposing (Payload)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, input)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Style
import Tracker.Msg exposing (Msg)


view : Payload -> Int -> ( Int, String ) -> Html Msg
view payload rowIndex ( cellIndex, str ) =
    Grid.column
        [ css [ Style.basicSpacing ] ]
        [ input
            [ css [ cellStyle payload rowIndex ]
            , Attrs.value str
            , Attrs.spellcheck False
            ]
            []
        ]


cellStyle : Payload -> Int -> Style
cellStyle payload rowIndex =
    [ outline none
    , determineCellBgColor payload rowIndex
        |> backgroundColor
    , Style.indent
    , Style.hftin
    , color Colors.point0
    , Css.width (px width)
    , Style.fontSmoothingNone
    ]
        |> Css.batch


determineCellBgColor : Payload -> Int -> Color
determineCellBgColor { majorMark, minorMark } rowIndex =
    if remainderBy majorMark rowIndex == 0 then
        Colors.background4
    else if remainderBy minorMark rowIndex == 0 then
        Colors.background3
    else
        Colors.background2


width : Float
width =
    60
