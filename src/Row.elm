module Row
    exposing
        ( Msg(..)
        , buttonStyle
        , setCell
        , view
        )

import Array exposing (Array)
import Cell
import Colors
import Css exposing (..)
import Data.Tracker as Tracker exposing (Tracker)
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Html
        , button
        )
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Html.Styled.Lazy
import Style


-- TYPES --


type Msg
    = CellMsg Int Cell.Msg



-- VIEW --


view : Int -> Int -> Tracker -> Int -> Array String -> Html Msg
view majorMark minorMark tracker index row =
    row
        |> Array.toIndexedList
        |> List.map (wrapCell majorMark minorMark tracker index)
        |> (::) (numberView tracker majorMark index)
        |> (::) (plusView tracker)
        |> (::) (deleteView tracker)
        |> Grid.row []


wrapCell : Int -> Int -> Tracker -> Int -> ( Int, String ) -> Html Msg
wrapCell majorMark minorMark tracker rowIndex ( cellIndex, str ) =
    Html.Styled.Lazy.lazy5
        Cell.view
        majorMark
        minorMark
        tracker
        rowIndex
        str
        |> Html.map (CellMsg cellIndex)


deleteView : Tracker -> Html Msg
deleteView tracker =
    button
        [ css [ buttonStyleClickable tracker ] ]
        [ Html.text "x" ]


plusView : Tracker -> Html Msg
plusView tracker =
    button
        [ css [ buttonStyleClickable tracker ] ]
        [ Html.text "+v" ]


numberView : Tracker -> Int -> Int -> Html Msg
numberView tracker majorMark index =
    button
        [ css [ numberStyle tracker ] ]
        [ Html.text (numberStr majorMark index) ]


numberStr : Int -> Int -> String
numberStr majorMark index =
    [ String.fromInt (index // majorMark)
    , "."
    , beatNumber
        (remainderBy majorMark index)
        "0123456789abcdefghijklmnopqrstuv"
    ]
        |> String.concat


beatNumber : Int -> String -> String
beatNumber i str =
    if i == 0 then
        String.left 1 str
    else
        beatNumber (i - 1) (String.dropLeft 1 str)


buttonStyleClickable : Tracker -> Style
buttonStyleClickable tracker =
    [ buttonStyle tracker
    , active [ Style.indent ]
    , hover [ color Colors.point1 ]
    ]
        |> Css.batch


buttonStyle : Tracker -> Style
buttonStyle tracker =
    [ Style.outdent
    , Tracker.font tracker
    , margin (px 1)
    , width (px (Tracker.cellWidth tracker / 2))
    , height (px (Tracker.cellHeight tracker))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]
        |> Css.batch


numberStyle : Tracker -> Style
numberStyle tracker =
    [ Style.outdent
    , Tracker.font tracker
    , margin (px 1)
    , width (px (Tracker.cellWidth tracker))
    , height (px (Tracker.cellHeight tracker))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]
        |> Css.batch



-- HELPERS --


setCell : Int -> String -> Array String -> Array String
setCell index str row =
    Array.set index str row
