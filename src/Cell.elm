module Cell
    exposing
        ( Msg(..)
        , view
        )

import Colors
import Css exposing (..)
import Data.Tracker as Tracker exposing (Tracker)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, input)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Html.Styled.Events exposing (onInput)
import Style


-- TYPES --


type Msg
    = Updated String



-- VIEW --


view : Int -> Int -> Tracker -> Int -> String -> Html Msg
view majorMark minorMark tracker rowIndex str =
    Grid.column
        [ css [ Style.basicSpacing ] ]
        [ input
            [ css [ style majorMark minorMark tracker rowIndex ]
            , Attrs.value str
            , Attrs.spellcheck False
            , onInput Updated
            ]
            []
        ]


style : Int -> Int -> Tracker -> Int -> Style
style majorMark minorMark tracker rowIndex =
    [ outline none
    , determineCellBgColor majorMark minorMark rowIndex
        |> backgroundColor
    , Style.indent
    , Tracker.font tracker
    , color Colors.point0
    , width (px (Tracker.cellWidth tracker))
    , Style.fontSmoothingNone
    ]
        |> Css.batch


determineCellBgColor : Int -> Int -> Int -> Color
determineCellBgColor majorMark minorMark rowIndex =
    if remainderBy majorMark rowIndex == 0 then
        Colors.background4
    else if remainderBy minorMark rowIndex == 0 then
        Colors.background3
    else
        Colors.background2
