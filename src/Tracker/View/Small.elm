module Tracker.View.Small
    exposing
        ( view
        )

import Array exposing (Array)
import Css exposing (..)
import Data.Sheet as Sheet exposing (Sheet)
import Data.Tracker exposing (Payload)
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        )
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Model exposing (Model)
import Style
import Tracker.Msg exposing (Msg)
import Tracker.View.Small.Cell as Cell
import Tracker.View.Small.Row as Row


-- VIEW --


view : Payload -> Html Msg
view payload =
    Grid.container
        [ css
            [ Style.card
            , flexDirection Css.column
            , height (calc (vh 100) minus (px 20))
            , overflow hidden
            ]
        ]
        [ Grid.row
            []
            (header payload)
        , Grid.row
            [ css [ marginTop (px 4) ] ]
            [ Grid.container
                [ css [ overflow auto ] ]
                (viewRows payload)
            ]
        ]


viewRows : Payload -> List (Html Msg)
viewRows payload =
    payload.sheet.rows
        |> Array.toIndexedList
        |> List.map (Row.view payload)



-- HEADER --


header : Payload -> List (Html Msg)
header { sheet } =
    List.range 0 (Sheet.columnCount sheet - 1)
        |> List.map (columnNumbers sheet)
        |> (::) (sheetNameView sheet)


columnNumbers : Sheet -> Int -> Html Msg
columnNumbers sheet i =
    Grid.column
        []
        [ button
            [ css
                [ Row.buttonStyle
                , width (px (Cell.width + 2))
                ]
            ]
            [ Html.text (String.fromInt i)
            ]
        ]


sheetNameView : Sheet -> Html Msg
sheetNameView sheet =
    Grid.column
        []
        [ button
            [ css
                [ Row.buttonStyle
                , width (px (Cell.width * 1.5 + 4))
                ]
            ]
            [ Html.text sheet.name ]
        ]
