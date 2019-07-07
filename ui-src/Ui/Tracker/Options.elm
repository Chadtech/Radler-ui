module Ui.Tracker.Options exposing
    ( Msg(..)
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Index exposing (Index)
import Data.Part exposing (Part)
import Data.Tracker as Tracker exposing (Tracker)
import Data.Tracker.Options as Options
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style
import View.Button as Button



-- TYPES --


type Msg
    = PartClicked (Index Part)
    | BackClicked
    | SmallClicked
    | BigClicked
    | MajorMarkFieldUpdated String
    | MinorMarkFieldUpdated String



-- UPDATE --


update : Index Tracker -> Msg -> Model -> Model
update trackerIndex =
    Model.mapTracker trackerIndex << updateTracker


updateTracker : Msg -> Tracker -> Tracker
updateTracker msg =
    case msg of
        PartClicked index ->
            Tracker.setPartIndex index

        BackClicked ->
            Tracker.closeOptions

        SmallClicked ->
            Tracker.setSize Style.Small

        BigClicked ->
            Tracker.setSize Style.Big

        MajorMarkFieldUpdated field ->
            Tracker.setMajorMark field

        MinorMarkFieldUpdated field ->
            Tracker.setMinorMark field



-- VIEW --


type alias ViewParams =
    { parts : List ( Index Part, String )
    , size : Style.Size
    , majorMark : Int
    , minorMark : Int
    , model : Options.Model
    }


view : ViewParams -> Html Msg
view params =
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
                    [ partOptionsContainer params ]
                ]
            , Grid.row
                [ margin (px 5) ]
                [ markLabel "major mark"
                , markField
                    params.model.majorMarkField
                    MajorMarkFieldUpdated
                ]
            , Grid.row
                [ margin (px 5) ]
                [ markLabel "minor mark"
                , markField
                    params.model.minorMarkField
                    MinorMarkFieldUpdated
                ]
            , Grid.row
                [ margin (px 5) ]
                [ smallViewButton params.size
                , bigViewButton params.size
                ]
            , Grid.row
                [ margin (px 5)
                , justifyContent spaceAround
                ]
                [ Button.button BackClicked "back"
                    |> Button.withWidth Button.fullWidth
                    |> Button.toHtml
                ]
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
        [ Button.button SmallClicked "small"
            |> Button.indent (size == Style.Small)
            |> Button.withWidth Button.fullWidth
            |> Button.toHtml
        ]


bigViewButton : Style.Size -> Html Msg
bigViewButton size =
    Grid.column
        [ paddingLeft (px 5) ]
        [ Button.button BigClicked "big"
            |> Button.indent (size == Style.Big)
            |> Button.withWidth Button.fullWidth
            |> Button.toHtml
        ]


partOptionsContainer : ViewParams -> Html Msg
partOptionsContainer params =
    Html.div
        [ Attrs.css
            [ Style.indent
            , backgroundColor Colors.background3
            , width (pct 100)
            ]
        ]
        [ Grid.container
            []
            (List.map partOptionView params.parts)
        ]


partOptionView : ( Index Part, String ) -> Html Msg
partOptionView ( index, name ) =
    let
        style : List Style
        style =
            [ Style.hfnss
            , marginLeft (px 10)
            , cursor pointer
            , width (pct 100)
            , hover
                [ backgroundColor Colors.background4
                , color Colors.point1
                ]
            ]
    in
    Grid.row
        [ Style.basicSpacing ]
        [ Grid.column
            []
            [ Html.p
                [ Attrs.css style
                , Events.onClick (PartClicked index)
                ]
                [ Html.text name ]
            ]
        ]
