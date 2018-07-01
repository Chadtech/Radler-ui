module Tracker.View.Small.Row
    exposing
        ( buttonStyle
        , view
        )

import Array exposing (Array)
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
import Tracker.Msg exposing (Msg)
import Tracker.View.Small.Cell as Cell


view : Payload -> ( Int, Array String ) -> Html Msg
view payload ( index, row ) =
    row
        |> Array.toIndexedList
        |> List.map (Cell.view payload index)
        |> (::) (numberView payload index)
        |> (::) (plusView payload index)
        |> (::) (deleteView payload index)
        |> Grid.row []


deleteView : Payload -> Int -> Html Msg
deleteView payload index =
    button
        [ css [ buttonStyleClickable ] ]
        [ Html.text "x" ]


plusView : Payload -> Int -> Html Msg
plusView payload index =
    button
        [ css [ buttonStyleClickable ] ]
        [ Html.text "+v" ]


numberView : Payload -> Int -> Html Msg
numberView payload index =
    button
        [ css [ buttonStyle ] ]
        [ Html.text (String.fromInt index) ]


buttonStyleClickable : Style
buttonStyleClickable =
    [ buttonStyle
    , active [ Style.indent ]
    , hover [ color Colors.point1 ]
    ]
        |> Css.batch


buttonStyle : Style
buttonStyle =
    [ Style.outdent
    , Style.hftin
    , margin (px 1)
    , width (px 30)
    , height (px 16)
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]
        |> Css.batch
