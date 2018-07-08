module Row
    exposing
        ( Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Cell
import Colors
import Css exposing (..)
import Data.Sheet as Sheet
import Data.Tracker as Tracker
import Html.Buttons as Buttons
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
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy
import Model exposing (Model)
import Style


-- TYPES --


type Msg
    = CellMsg Int Cell.Msg
    | DeleteClicked
    | AddBelowClicked



-- UPDATE --


update : Int -> Int -> Msg -> Model -> Model
update si ri msg model =
    case msg of
        CellMsg ci subMsg ->
            Cell.update si ri ci subMsg model

        DeleteClicked ->
            Model.mapSheet
                si
                (Sheet.removeRow ri)
                model

        AddBelowClicked ->
            Model.mapSheet
                si
                (Sheet.addRow ri)
                model



-- VIEW --


view : Int -> Int -> Style.Size -> Int -> Array String -> Html Msg
view majorMark minorMark size index row =
    row
        |> Array.toIndexedList
        |> List.map (wrapCell majorMark minorMark size index)
        |> (::) (numberView size majorMark index)
        |> (::) (Buttons.plus AddBelowClicked [] size)
        |> (::) (Buttons.delete DeleteClicked size)
        |> Grid.row []


wrapCell : Int -> Int -> Style.Size -> Int -> ( Int, String ) -> Html Msg
wrapCell majorMark minorMark size rowIndex ( cellIndex, str ) =
    Html.Styled.Lazy.lazy5
        Cell.view
        majorMark
        minorMark
        size
        rowIndex
        str
        |> Html.map (CellMsg cellIndex)


numberView : Style.Size -> Int -> Int -> Html Msg
numberView size majorMark index =
    button
        [ css [ numberStyle size ] ]
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


numberStyle : Style.Size -> Style
numberStyle size =
    [ Style.outdent
    , Style.font size
    , margin (px 1)
    , width (px (Style.cellWidth size))
    , height (px (Style.cellHeight size))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]
        |> Css.batch
