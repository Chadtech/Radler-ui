module Ui.Tracker.Options exposing
    ( Msg(..)
    , Payload
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Tracker as Tracker
import Data.Tracker.Options as Options
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style



-- TYPES --


type alias Payload =
    { parts : List ( Int, String )
    , size : Style.Size
    , majorMark : Int
    , minorMark : Int
    , model : Options.Model
    }


type Msg
    = PartClicked Int
    | BackClicked
    | SmallClicked
    | BigClicked
    | MajorMarkFieldUpdated String
    | MinorMarkFieldUpdated String
    | CopyWithNameClicked
    | CopyNameChanged String



-- UPDATE --


{-|

    Theres a lot of indexing going on!

        ti := tracker index
        pi := part index
        bi := beat index

-}
update : Int -> Int -> Options.Model -> Msg -> Model -> Model
update ti pi options msg =
    case msg of
        PartClicked index ->
            index
                |> Tracker.setPartIndex
                |> Model.mapTracker ti

        BackClicked ->
            Tracker.closeOptions
                |> Model.mapTracker ti

        SmallClicked ->
            Style.Small
                |> Tracker.setSize
                |> Model.mapTracker ti

        BigClicked ->
            Style.Big
                |> Tracker.setSize
                |> Model.mapTracker ti

        MajorMarkFieldUpdated field ->
            field
                |> Tracker.setMajorMark
                |> Model.mapTracker ti

        MinorMarkFieldUpdated field ->
            field
                |> Tracker.setMinorMark
                |> Model.mapTracker ti

        CopyWithNameClicked ->
            Model.copyPart pi options.copyName

        CopyNameChanged copyName ->
            copyName
                |> Options.setCopyName
                |> Tracker.mapOptionsModel
                |> Model.mapTracker ti



-- VIEW --


view : Payload -> Html Msg
view payload =
    Html.div
        [ Attrs.css
            [ Style.card
            , transform (translate2 (pct -50) (pct -50))
            , position absolute
            , top (pct 50)
            , left (pct 50)
            , minWidth (px 375)
            ]
        ]
        [ Grid.container
            [ margin (px 0)
            , width (pct 100)
            ]
            [ Grid.row
                [ margin (px 5) ]
                [ Grid.column
                    []
                    [ Html.p
                        [ Attrs.css
                            [ Style.hfnss
                            , padding (px 5)
                            , backgroundColor Colors.point0
                            , color Colors.ignorable2
                            , width (pct 100)
                            ]
                        ]
                        [ Html.text "tracker options" ]
                    ]
                ]
            , Grid.row
                [ margin (px 5) ]
                [ Grid.column
                    []
                    [ Html.p
                        [ Attrs.css
                            [ Style.hfnss ]
                        ]
                        [ Html.text "switch to part.." ]
                    ]
                ]
            , Grid.row
                [ margin (px 5) ]
                [ Grid.column
                    []
                    [ partOptionsContainer payload ]
                ]
            , Grid.row
                [ margin (px 5) ]
                [ markLabel "major mark"
                , markField
                    payload.model.majorMarkField
                    MajorMarkFieldUpdated
                ]
            , Grid.row
                [ margin (px 5) ]
                [ markLabel "minor mark"
                , markField
                    payload.model.minorMarkField
                    MinorMarkFieldUpdated
                ]
            , Grid.row
                [ margin (px 5) ]
                [ smallViewButton payload.size
                , bigViewButton payload.size
                ]
            , Grid.row
                [ margin (px 5) ]
                (copyWithNameField payload)
            , Grid.row
                [ margin (px 5)
                , justifyContent spaceAround
                ]
                [ backButton ]
            ]
        ]


copyWithNameField : Payload -> List (Html Msg)
copyWithNameField { model } =
    [ Grid.column
        []
        [ Html.input
            [ Attrs.css
                [ Style.hfnss
                , color Colors.point0
                , Style.fontSmoothingNone
                , width (pct 100)
                ]
            , Events.onInput CopyNameChanged
            , Attrs.value model.copyName
            ]
            []
        ]
    , Grid.column
        [ paddingLeft (px 5) ]
        [ Html.button
            [ Attrs.css
                [ buttonStyle
                , margin (px 0)
                , width (pct 100)
                ]
            , Events.onClick CopyWithNameClicked
            ]
            [ Html.text "copy with name" ]
        ]
    ]


markLabel : String -> Html Msg
markLabel labelText =
    Grid.column
        []
        [ Html.p
            [ Attrs.css
                [ Style.hfnss
                , lineHeight (px 26)
                ]
            ]
            [ Html.text labelText ]
        ]


markField : String -> (String -> Msg) -> Html Msg
markField mark msgCtor =
    Grid.column
        [ paddingLeft (px 5) ]
        [ Html.input
            [ Attrs.css
                [ Style.hfnss
                , color Colors.point0
                , Style.fontSmoothingNone
                , width (pct 100)
                ]
            , Events.onInput msgCtor
            , Attrs.value mark
            ]
            []
        ]


smallViewButton : Style.Size -> Html Msg
smallViewButton size =
    Grid.column
        []
        [ Html.button
            [ Attrs.css
                [ buttonStyle
                , indentIf (size == Style.Small)
                , margin (px 0)
                , width (pct 100)
                ]
            , Events.onClick SmallClicked
            ]
            [ Html.text "small" ]
        ]


bigViewButton : Style.Size -> Html Msg
bigViewButton size =
    Grid.column
        [ paddingLeft (px 5) ]
        [ Html.button
            [ Attrs.css
                [ buttonStyle
                , indentIf (size == Style.Big)
                , margin (px 0)
                , width (pct 100)
                ]
            , Events.onClick BigClicked
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
    Html.div
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
            [ Html.p
                [ Attrs.css [ partOptionStyle ]
                , Events.onClick (PartClicked index)
                ]
                [ Html.text name ]
            ]
        ]


partOptionStyle : Style
partOptionStyle =
    [ Style.hfnss
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
        [ Html.button
            [ Attrs.css [ buttonStyle ]
            , Events.onClick BackClicked
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
