module Row
    exposing
        ( Msg
        , buttonStyle
        , view
        )

import Array exposing (Array)
import Cell
import Colors
import Css exposing (..)
import Data.Tracker exposing (Payload)
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
import Style


-- TYPES --


type Msg
    = CellMsg Int Cell.Msg



-- VIEW --


view : Payload -> ( Int, Array String ) -> Html Msg
view payload ( index, row ) =
    row
        |> Array.toIndexedList
        |> List.map (wrapCell payload index)
        |> (::) (numberView payload index)
        |> (::) (plusView payload index)
        |> (::) (deleteView payload index)
        |> Grid.row []


wrapCell : Payload -> Int -> ( Int, String ) -> Html Msg
wrapCell payload rowIndex ( cellIndex, str ) =
    Cell.view payload rowIndex ( cellIndex, str )
        |> Html.map (CellMsg cellIndex)


deleteView : Payload -> Int -> Html Msg
deleteView payload index =
    button
        [ css [ buttonStyleClickable payload ] ]
        [ Html.text "x" ]


plusView : Payload -> Int -> Html Msg
plusView payload index =
    button
        [ css [ buttonStyleClickable payload ] ]
        [ Html.text "+v" ]


numberView : Payload -> Int -> Html Msg
numberView payload index =
    button
        [ css [ buttonStyle payload ] ]
        [ Html.text (String.fromInt index) ]


buttonStyleClickable : Payload -> Style
buttonStyleClickable payload =
    [ buttonStyle payload
    , active [ Style.indent ]
    , hover [ color Colors.point1 ]
    ]
        |> Css.batch


buttonStyle : Payload -> Style
buttonStyle payload =
    [ Style.outdent
    , payload.fontStyle
    , margin (px 1)
    , width (px (payload.cellWidth / 2))
    , height (px payload.cellHeight)
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]
        |> Css.batch
