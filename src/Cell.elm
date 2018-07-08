module Cell
    exposing
        ( Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Browser.Dom as Dom
import Colors
import Css exposing (..)
import Data.Sheet as Sheet
import Data.Tracker as Tracker
import Html.Custom exposing (Arrow(..), onArrowKey, onEnter)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, input)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Html.Styled.Events exposing (onInput)
import Model exposing (Model)
import Return2 as R2
import Style
import Task


-- TYPES --


type Msg
    = Updated String
    | ArrowPressed Arrow
    | EnterPressed
    | CellFocused



-- UPDATE --


update : Int -> Int -> Int -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update ti si ri ci msg model =
    case msg of
        Updated str ->
            setCell ci str
                |> Sheet.mapRow ri
                |> Model.mapSheet si
                |> (|>) model
                |> R2.withNoCmd

        ArrowPressed Up ->
            cellId ti (ri - 1) ci
                |> focusOnCell
                |> R2.withModel model

        ArrowPressed Down ->
            cellId ti (ri + 1) ci
                |> focusOnCell
                |> R2.withModel model

        ArrowPressed Left ->
            cellId ti ri (ci - 1)
                |> focusOnCell
                |> R2.withModel model

        ArrowPressed Right ->
            cellId ti ri (ci + 1)
                |> focusOnCell
                |> R2.withModel model

        CellFocused ->
            model
                |> R2.withNoCmd

        EnterPressed ->
            cellId ti (ri + 1) ci
                |> focusOnCell
                |> R2.withModel model


focusOnCell : String -> Cmd Msg
focusOnCell id =
    Task.attempt (always CellFocused) (Dom.focus id)


setCell : Int -> String -> Array String -> Array String
setCell index str row =
    Array.set index str row



-- VIEW --


view : Int -> Int -> Style.Size -> Int -> Int -> Int -> String -> Html Msg
view majorMark minorMark size ti ri ci str =
    Grid.column
        [ margin (px 1)
        , marginBottom (px 0)
        ]
        [ input
            [ css
                [ style
                    majorMark
                    minorMark
                    size
                    ri
                ]
            , Attrs.value str
            , Attrs.spellcheck False
            , onInput Updated
            , onArrowKey ArrowPressed
            , onEnter EnterPressed
            , Attrs.id (cellId ti ri ci)
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


cellId : Int -> Int -> Int -> String
cellId ti ri ci =
    [ "t"
    , String.fromInt ti
    , "r"
    , String.fromInt ri
    , "c"
    , String.fromInt ci
    ]
        |> String.concat
