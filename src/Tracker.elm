module Tracker
    exposing
        ( Msg(..)
        , view
        )

import Array exposing (Array)
import Css exposing (..)
import Data.Sheet as Sheet exposing (Sheet)
import Data.Tracker
    exposing
        ( Payload
        , Tracker(..)
        )
import Html.Custom exposing (p)
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        )
import Html.Styled.Attributes exposing (css)
import Model exposing (Model)
import Row
import Style


-- TYPES --


type Msg
    = RowMsg Int Row.Msg



-- VIEW --


view : Model -> ( Int, Tracker ) -> Html Msg
view model ( sheetIndex, tracker ) =
    case Array.get sheetIndex model.sheets of
        Just sheet ->
            case tracker of
                Big ->
                    { sheet = sheet
                    , majorMark = model.majorMark
                    , minorMark = model.minorMark
                    , fontStyle = Css.batch [ Style.hfnss ]
                    , cellWidth = 90
                    , cellHeight = 26
                    }
                        |> fromPayload

                Small ->
                    { sheet = sheet
                    , majorMark = model.majorMark
                    , minorMark = model.minorMark
                    , fontStyle = Css.batch [ Style.hftin ]
                    , cellWidth = 60
                    , cellHeight = 16
                    }
                        |> fromPayload

        Nothing ->
            notFoundView


fromPayload : Payload -> Html Msg
fromPayload payload =
    Grid.container
        [ css
            [ Style.card
            , flexDirection Css.column
            , height (calc (vh 100) minus (px 68))
            , overflow hidden
            ]
        ]
        [ Grid.row
            [ css [ minHeight fitContent ] ]
            (header payload)
        , Grid.row
            []
            [ Grid.container
                [ css [ overflow auto ] ]
                (viewRows payload)
            ]
        ]


viewRows : Payload -> List (Html Msg)
viewRows payload =
    payload.sheet.rows
        |> Array.toIndexedList
        |> List.map (wrapRow payload)


wrapRow : Payload -> ( Int, Array String ) -> Html Msg
wrapRow payload ( index, row ) =
    Row.view payload ( index, row )
        |> Html.map (RowMsg index)



-- HEADER --


header : Payload -> List (Html Msg)
header payload =
    List.range 0 (Sheet.columnCount payload.sheet - 1)
        |> List.map (columnNumbers payload)
        |> (::) (sheetNameView payload)


columnNumbers : Payload -> Int -> Html Msg
columnNumbers ({ cellWidth, sheet } as payload) i =
    Grid.column
        []
        [ button
            [ css
                [ Row.buttonStyle payload
                , width (px (cellWidth + 2))
                , minHeight fitContent
                ]
            ]
            [ Html.text (String.fromInt i)
            ]
        ]


sheetNameView : Payload -> Html Msg
sheetNameView ({ cellWidth, sheet } as payload) =
    Grid.column
        []
        [ button
            [ css
                [ Row.buttonStyle payload
                , width (px (cellWidth * 1.5 + 4))
                ]
            ]
            [ Html.text sheet.name ]
        ]



-- NOT FOUND VIEW --


notFoundView : Html Msg
notFoundView =
    div
        [ css [ Style.card ] ]
        [ p
            [ css
                [ Style.basicP
                , Style.hfnss
                , whiteSpace noWrap
                , margin (px 4)
                ]
            ]
            [ Html.text "Error : Sheet not found" ]
        ]
