module Small
    exposing
        ( view
        )

import Array exposing (Array)
import Colors
import Css exposing (..)
import Data.Sheet as Sheet exposing (Sheet)
import Html.Custom exposing (p)
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , input
        )
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style


-- VIEW --


view : Model -> Html Msg
view model =
    div
        [ css containerStyle ]
        (viewBody model)


containerStyle : List Style
containerStyle =
    [ Style.outdent
    , backgroundColor Colors.ignorable2
    , flex2 (int 0) (int 1)
    , margin (px 4)
    ]


viewBody : Model -> List (Html Msg)
viewBody model =
    case Array.get model.bigViewSheet model.sheets of
        Just sheet ->
            viewSheet sheet model

        Nothing ->
            [ p
                [ css [ Style.hftin ] ]
                [ Html.text "Error : No Sheet" ]
            ]


viewSheet : Sheet -> Model -> List (Html Msg)
viewSheet sheet model =
    sheet.rows
        |> Array.toIndexedList
        |> List.map (rowView model)
        |> (::) (rowHeadersView sheet)


rowHeadersView : Sheet -> Html Msg
rowHeadersView sheet =
    List.range 0 (Sheet.columnCount sheet)
        |> List.map rowHeaderView
        |> div [ css rowStyle ]


rowHeaderView : Int -> Html Msg
rowHeaderView int =
    Html.text (String.fromInt int)


rowView : Model -> ( Int, Array String ) -> Html Msg
rowView model ( index, row ) =
    row
        |> Array.toIndexedList
        |> List.map (cellView model index)
        |> (::) (rowNumberView model index)
        |> (::) (rowPlusView model index)
        |> (::) (rowDeleteView model index)
        |> div [ css rowStyle ]


rowStyle : List Style
rowStyle =
    [ displayFlex
    ]


rowDeleteView : Model -> Int -> Html Msg
rowDeleteView model index =
    button
        [ css rowButtonStyle ]
        [ Html.text "x" ]


rowPlusView : Model -> Int -> Html Msg
rowPlusView model index =
    button
        [ css rowButtonStyle ]
        [ Html.text "+v" ]


rowButtonStyle : List Style
rowButtonStyle =
    [ Style.outdent
    , Style.hftin
    , active [ Style.indent ]
    , hover [ color Colors.point1 ]
    , margin (px 1)
    , flex2 (int 1) (int 1)
    , width (px 30)
    , height (px 16)
    , lineHeight (px 12)
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    ]


rowNumberView : Model -> Int -> Html Msg
rowNumberView model index =
    p
        [ css rowNumberStyle ]
        [ Html.text (String.fromInt index) ]


rowNumberStyle : List Style
rowNumberStyle =
    [ Style.outdent
    , Style.hftin
    , margin (px 1)
    , textAlign center
    , height (px 12)
    , flex2 (int 1) (int 1)
    , width (px 30)
    , lineHeight (px 14)
    ]


cellView : Model -> Int -> ( Int, String ) -> Html Msg
cellView model rowIndex ( cellIndex, str ) =
    input
        [ css cellStyle
        , Attrs.value str
        , Attrs.spellcheck False
        ]
        []


cellStyle : List Style
cellStyle =
    [ Style.basicInput
    , Style.hftin
    , color Colors.point0
    , width (px cellWidth)
    , margin (px 1)
    , flex2 (int 0) (int 1)
    , Style.fontSmoothingNone
    ]


cellWidth : Float
cellWidth =
    60
