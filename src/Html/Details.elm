module Html.Details
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
    { partNameField : String
    , parts : List ( Int, String )
    , size : Style.Size
    , majorMark : Int
    , minorMark : Int
    }


type Msg
    = NameFieldUpdated String
    | PartClicked Int
    | BackClicked
    | SmallClicked
    | BigClicked
    | MajorMarkFieldUpdated String
    | MinorMarkFieldUpdated String



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
                (partNameInput payload)
            , Grid.row
                [ margin (px 5) ]
                [ Grid.column
                    []
                    [ partOptions payload ]
                ]
            , Grid.row
                [ margin (px 5) ]
                [ markLabel "major mark"
                , markField
                    payload.majorMark
                    MajorMarkFieldUpdated
                ]
            , Grid.row
                [ margin (px 5) ]
                [ markLabel "minor mark"
                , markField
                    payload.minorMark
                    MinorMarkFieldUpdated
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
                [ partBackButton ]
            ]
        ]


markLabel : String -> Html Msg
markLabel labelText =
    Grid.column
        []
        [ p
            [ css
                [ Style.basicP
                , Style.hfnss
                , lineHeight (px 26)
                , paddingLeft (px 10)
                ]
            ]
            [ Html.text labelText ]
        ]


markField : Int -> (String -> Msg) -> Html Msg
markField mark msgCtor =
    Grid.column
        [ paddingLeft (px 5) ]
        [ input
            [ css
                [ Style.basicInput
                , Style.hfnss
                , color Colors.point0
                , Style.fontSmoothingNone
                , width (pct 100)
                ]
            , onInput msgCtor
            , Attrs.defaultValue (String.fromInt mark)
            ]
            []
        ]


smallViewButton : Style.Size -> Html Msg
smallViewButton size =
    Grid.column
        []
        [ button
            [ css
                [ buttonStyle
                , indentIf (size == Style.Small)
                , margin (px 0)
                , width (pct 100)
                ]
            , onClick SmallClicked
            ]
            [ Html.text "small" ]
        ]


bigViewButton : Style.Size -> Html Msg
bigViewButton size =
    Grid.column
        [ paddingLeft (px 5) ]
        [ button
            [ css
                [ buttonStyle
                , indentIf (size == Style.Big)
                , margin (px 0)
                , width (pct 100)
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


partOptions : Payload -> Html Msg
partOptions payload =
    div
        [ css
            [ Style.indent
            , backgroundColor Colors.background3
            , width (pct 100)
            ]
        ]
        [ Grid.container
            []
            (List.map partOptionView payload.parts)
        ]


partOptionView : ( Int, String ) -> Html Msg
partOptionView ( index, name ) =
    Grid.row
        [ Style.basicSpacing ]
        [ Grid.column
            []
            [ p
                [ css [ partOptionStyle ]
                , onClick (PartClicked index)
                ]
                [ Html.text name ]
            ]
        ]


partOptionStyle : Style
partOptionStyle =
    [ Style.hfnss
    , marginLeft (px 10)
    , cursor pointer
    , hover
        [ backgroundColor Colors.background4
        , color Colors.point1
        ]
    ]
        |> Css.batch


partNameInput : Payload -> List (Html Msg)
partNameInput payload =
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
            [ Html.text "part name" ]
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
            , Attrs.value payload.partNameField
            , Attrs.spellcheck False
            , onInput NameFieldUpdated
            ]
            []
        ]
    ]


partBackButton : Html Msg
partBackButton =
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
    , width (px (Style.noteWidth Style.Big * 2))
    , cursor pointer
    , hover [ color Colors.point1 ]
    , active [ Style.indent ]
    ]
        |> Css.batch
