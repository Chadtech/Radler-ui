module Cell
    exposing
        ( Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Colors
import Css exposing (..)
import Data.Sheet as Sheet
import Data.Tracker as Tracker
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, input)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Html.Styled.Events exposing (onInput)
import Model exposing (Model)
import Style


-- TYPES --


type Msg
    = Updated String



-- UPDATE --


update : Int -> Int -> Int -> Msg -> Model -> Model
update si ri ci msg model =
    case msg of
        Updated str ->
            setCell ci str
                |> Sheet.mapRow ri
                |> Model.mapSheet si
                |> (|>) model


setCell : Int -> String -> Array String -> Array String
setCell index str row =
    Array.set index str row



-- VIEW --


view : Int -> Int -> Style.Size -> Int -> String -> Html Msg
view majorMark minorMark size rowIndex str =
    Grid.column
        [ Style.basicSpacing ]
        [ input
            [ css
                [ style
                    majorMark
                    minorMark
                    size
                    rowIndex
                ]
            , Attrs.value str
            , Attrs.spellcheck False
            , onInput Updated
            ]
            []
        ]


style : Int -> Int -> Style.Size -> Int -> Style
style majorMark minorMark size rowIndex =
    [ outline none
    , determineCellBgColor
        majorMark
        minorMark
        rowIndex
        |> backgroundColor
    , Style.indent
    , Style.font size
    , color Colors.point0
    , width (px (Style.cellWidth size))
    , Style.fontSmoothingNone
    ]
        |> Css.batch


determineCellBgColor : Int -> Int -> Int -> Color
determineCellBgColor majorMark minorMark rowIndex =
    if remainderBy majorMark rowIndex == 0 then
        Colors.highlight1
    else if remainderBy minorMark rowIndex == 0 then
        Colors.highlight0
    else
        Colors.background3
