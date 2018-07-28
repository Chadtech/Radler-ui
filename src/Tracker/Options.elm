module Tracker.Options
    exposing
        ( Msg(..)
        , Payload
        , update
        , view
        )

--import Row

import Colors
import Css exposing (..)
import Data.Part as Part
import Data.Tracker as Tracker
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , input
        , p
        )
import Html.Styled.Attributes as Attrs
import Html.Styled.Events
    exposing
        ( onClick
        , onInput
        )
import Model exposing (Model)
import Style


-- TYPES --


type alias Payload =
    { parts : List ( Int, String )
    , size : Style.Size
    , majorMark : Int
    , minorMark : Int
    }


type Msg
    = PartClicked Int
    | BackClicked
    | SmallClicked
    | BigClicked
    | MajorMarkFieldUpdated String
    | MinorMarkFieldUpdated String



-- UPDATE --


{-|

    Theres a lot of indexing going on!

        ti := tracker index
        pi := part index

-}
update : Int -> Int -> Msg -> Model -> Model
update ti pi msg model =
    case msg of
        PartClicked index ->
            Model.mapTracker
                ti
                (Tracker.setPartIndex index)
                model

        BackClicked ->
            Model.mapTracker
                ti
                Tracker.closeOptions
                model

        SmallClicked ->
            Model.mapTracker
                ti
                (Tracker.setSize Style.Small)
                model

        BigClicked ->
            Model.mapTracker
                ti
                (Tracker.setSize Style.Big)
                model

        MajorMarkFieldUpdated field ->
            case String.toInt field of
                Just majorMark ->
                    Model.mapTracker
                        ti
                        (Tracker.setMajorMark majorMark)
                        model

                Nothing ->
                    model

        MinorMarkFieldUpdated field ->
            case String.toInt field of
                Just minorMark ->
                    Model.mapTracker
                        ti
                        (Tracker.setMinorMark minorMark)
                        model

                Nothing ->
                    model



-- VIEW --


view : Payload -> Html Msg
view payload =
    div
        [ Attrs.css
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
                [ Grid.column
                    []
                    [ partOptionsContainer payload ]
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
                [ backButton ]
            ]
        ]


markLabel : String -> Html Msg
markLabel labelText =
    Grid.column
        []
        [ p
            [ Attrs.css
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
            [ Attrs.css
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
            [ Attrs.css
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
            [ Attrs.css
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


partOptionsContainer : Payload -> Html Msg
partOptionsContainer payload =
    div
        [ Attrs.css
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
                [ Attrs.css [ partOptionStyle ]
                , onClick (PartClicked index)
                ]
                [ Html.text name ]
            ]
        ]


partOptionStyle : Style
partOptionStyle =
    [ Style.basicP
    , Style.hfnss
    , marginLeft (px 10)
    , cursor pointer
    , width (pct 100)
    , hover
        [ backgroundColor Colors.background4
        , color Colors.point1
        ]
    ]
        |> Css.batch


backButton : Html Msg
backButton =
    Grid.column
        []
        [ button
            [ Attrs.css [ buttonStyle ]
            , onClick BackClicked
            ]
            [ Html.text "back" ]
        ]


buttonStyle : Style
buttonStyle =
    [ Style.basicButton Style.Big
    , width (pct 100)
    , cursor pointer
    , hover [ color Colors.point1 ]
    , active [ Style.indent ]
    ]
        |> Css.batch
