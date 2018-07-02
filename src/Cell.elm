module Cell
    exposing
        ( Msg
        , view
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


-- TYPES --


type Msg
    = Noop



-- VIEW --


view : Payload -> Int -> ( Int, String ) -> Html Msg
view payload rowIndex ( cellIndex, str ) =
    Grid.column
        [ css [ Style.basicSpacing ] ]
        [ input
            [ css [ style payload rowIndex ]
            , Attrs.value str
            , Attrs.spellcheck False
            ]
            []
        ]


style : Payload -> Int -> Style
style payload rowIndex =
    [ outline none
    , determineCellBgColor payload rowIndex
        |> backgroundColor
    , Style.indent
    , payload.fontStyle
    , color Colors.point0
    , Css.width (px payload.cellWidth)
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
