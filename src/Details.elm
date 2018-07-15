module Details
    exposing
        ( Msg(..)
        , Payload
        , view
        )

--import Row

import Colors
import Css exposing (..)
import Html.Custom exposing (p)
import Html.Grid as Grid
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
import Html.Styled.Events
    exposing
        ( onClick
        , onInput
        )
import Style


-- TYPES --


type alias Payload =
    { sheetNameField : String
    , sheets : List ( Int, String )
    , size : Style.Size
    }


type Msg
    = NameFieldUpdated String
    | SheetClicked Int
    | BackClicked
    | SmallClicked
    | BigClicked



-- VIEW --


view : Payload -> Html Msg
view payload =
    div
        [ css
            [ Style.card
            , transform (translate2 (pct -50) (pct -50))
            , position absolute
            , top (pct 50)
            , left (pct 50)
            ]
        ]
        [ Grid.container
            []
            [ Grid.row
                [ margin (px 5) ]
                (sheetNameInput payload)
            , Grid.row
                [ margin (px 5) ]
                [ Grid.column
                    []
                    [ sheetOptions payload ]
                ]
            , Grid.row
                [ margin (px 5) ]
                [ smallViewButton payload.size
                , bigViewButton payload.size
                ]
            , Grid.row
                [ margin (px 5)
                , justifyContent spaceAround
                ]
                [ sheetBackButton ]
            ]
        ]


smallViewButton : Style.Size -> Html Msg
smallViewButton size =
    Grid.column
        []
        [ button
            [ css
                [ buttonStyle
                , indentIf (size == Style.Small)
                ]
            , onClick SmallClicked
            ]
            [ Html.text "small" ]
        ]


bigViewButton : Style.Size -> Html Msg
bigViewButton size =
    Grid.column
        []
        [ button
            [ css
                [ buttonStyle
                , indentIf (size == Style.Big)
                ]
            , onClick BigClicked
            ]
            [ Html.text "big" ]
        ]


indentIf : Bool -> Style
indentIf condition =
    if condition then
        Style.indent
    else
        Css.batch []


sheetOptions : Payload -> Html Msg
sheetOptions payload =
    div
        [ css
            [ Style.indent
            , backgroundColor Colors.background3
            ]
        ]
        [ Grid.container
            []
            (List.map sheetOptionView payload.sheets)
        ]


sheetOptionView : ( Int, String ) -> Html Msg
sheetOptionView ( index, name ) =
    Grid.row
        [ Style.basicSpacing ]
        [ Grid.column
            []
            [ p
                [ css [ sheetOptionStyle ]
                , onClick (SheetClicked index)
                ]
                [ Html.text name ]
            ]
        ]


sheetOptionStyle : Style
sheetOptionStyle =
    [ Style.hfnss
    , marginLeft (px 10)
    , cursor pointer
    , hover
        [ backgroundColor Colors.background4
        , color Colors.point1
        ]
    ]
        |> Css.batch


sheetNameInput : Payload -> List (Html Msg)
sheetNameInput payload =
    [ Grid.column
        [ lineHeight (px 26)
        , Style.basicSpacing
        ]
        [ p
            [ css
                [ Style.hfnss
                , marginRight (px 10)
                , marginLeft (px 10)
                , whiteSpace noWrap
                ]
            ]
            [ Html.text "sheet name" ]
        ]
    , Grid.column
        [ Style.basicSpacing ]
        [ input
            [ css
                [ Style.basicInput
                , Style.hfnss
                , color Colors.point0
                , Style.fontSmoothingNone
                ]
            , Attrs.value payload.sheetNameField
            , Attrs.spellcheck False
            , onInput NameFieldUpdated
            ]
            []
        ]
    ]


sheetBackButton : Html Msg
sheetBackButton =
    Grid.column
        [ flex none ]
        [ button
            [ css [ buttonStyle ]
            , onClick BackClicked
            ]
            [ Html.text "back" ]
        ]


buttonStyle : Style
buttonStyle =
    [ Style.basicButton Style.Big
    , width (px (Style.cellWidth Style.Big * 2))
    , cursor pointer
    , hover [ color Colors.point1 ]
    , active [ Style.indent ]
    ]
        |> Css.batch
