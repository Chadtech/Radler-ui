module Ui.Tracker exposing
    ( Msg
    , update
    , view
    )

import Array exposing (Array)
import Colors
import Css exposing (..)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker exposing (Tracker)
import Data.Tracker.Options
import Html.Buttons as Buttons
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Lazy
import Model exposing (Model)
import Style
import Ui.Beat as Beat
import Ui.Tracker.Options
import Util.Cmd as CmdUtil



-- TYPES --


type Msg
    = BeatMsg Int Beat.Msg
    | OptionsMsg Ui.Tracker.Options.Msg
    | OptionsClicked
    | DeleteTrackerClicked
    | AddBeatBelowClicked
    | DeleteVoiceClicked Int
    | AddVoiceClicked Int



-- UPDATE --


{-|

    Theres a lot of indexing going on!

        ti := tracker index
        pi := part index
        bi := beat index

-}
update : Int -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update ti pi msg model =
    case msg of
        OptionsClicked ->
            case Model.getTrackersPart ti model of
                Just part ->
                    Model.mapTracker
                        ti
                        (Tracker.openOptions part.name)
                        model
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        OptionsMsg subMsg ->
            case Model.getTrackersOptions ti model of
                Just options ->
                    Ui.Tracker.Options.update
                        ti
                        pi
                        options
                        subMsg
                        model
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        BeatMsg bi subMsg ->
            Beat.update ti pi bi subMsg model
                |> CmdUtil.mapCmd (BeatMsg bi)

        DeleteTrackerClicked ->
            Model.removeTracker ti model
                |> CmdUtil.withNoCmd

        AddBeatBelowClicked ->
            Model.mapPart
                pi
                Part.addBeatToBeginning
                model
                |> CmdUtil.withNoCmd

        AddVoiceClicked index ->
            Model.mapPart
                pi
                (Part.addVoice index)
                model
                |> CmdUtil.withNoCmd

        DeleteVoiceClicked index ->
            Model.mapPart
                pi
                (Part.removeVoice index)
                model
                |> CmdUtil.withNoCmd



-- VIEW --


type alias ViewParams =
    { trackerIndex : Int
    , tracker : Tracker
    , part : Part
    , partNames : List ( Int, String )
    }


view : ViewParams -> Html Msg
view { trackerIndex, tracker, part, partNames } =
    Grid.container
        [ Style.card
        , flexDirection Css.column
        , height (calc (vh 100) minus (px 95))
        , overflow hidden
        , position relative
        ]
        [ optionsContainerView tracker partNames
        , Grid.row
            [ minHeight fitContent ]
            (trackerOptionsRow part tracker.size)
        , Grid.row
            [ minHeight fitContent ]
            (voiceOptions part tracker.size)
        , Grid.row
            [ minHeight fitContent ]
            (voiceNumbers part tracker.size)
        , Grid.row
            []
            [ Html.Styled.Lazy.lazy5
                beatsView
                part
                tracker.majorMark
                tracker.minorMark
                tracker.size
                trackerIndex
            ]
        ]


optionsContainerView : Tracker -> List ( Int, String ) -> Html Msg
optionsContainerView tracker partNames =
    case tracker.options of
        Just options ->
            let
                style : List Style
                style =
                    [ Style.dim
                    , width (pct 100)
                    , height (pct 100)
                    , position absolute
                    , zIndex (int 2)
                    ]
            in
            Html.div
                [ Attrs.css style ]
                [ Ui.Tracker.Options.view
                    { parts = partNames
                    , size = tracker.size
                    , model = options
                    , majorMark = tracker.majorMark
                    , minorMark = tracker.minorMark
                    }
                ]
                |> Html.map OptionsMsg

        Nothing ->
            Html.text ""


beatsView : Part -> Int -> Int -> Style.Size -> Int -> Html Msg
beatsView part majorMark minorMark size ti =
    let
        wrapBeat : ( Int, Beat Encoding.None ) -> Html Msg
        wrapBeat ( bi, beat ) =
            Html.Styled.Lazy.lazy6
                Beat.view
                majorMark
                minorMark
                size
                ti
                bi
                beat
                |> Html.map (BeatMsg bi)
    in
    part.beats
        |> Array.toIndexedList
        |> List.map wrapBeat
        |> Grid.container [ overflow auto ]



-- TRACKER OPTIONS --


{-|

    The options on the very top of each tracker
    view, including the X button the close the
    tracker and the name of the part, which can
    be clicked to reveal options about this tracker

-}
trackerOptionsRow : Part -> Style.Size -> List (Html Msg)
trackerOptionsRow part size =
    let
        buttonWidth : Float
        buttonWidth =
            Style.noteWidth size + 3

        trackerOptionsButton : Html Msg
        trackerOptionsButton =
            Html.button
                [ Attrs.css
                    [ Style.basicButton size
                    , width (px ((Style.noteWidth size * 2) + 2))
                    ]
                , Events.onClick OptionsClicked
                ]
                [ Html.text "options" ]
    in
    [ Grid.column
        [ flex none
        , flexBasis (px buttonWidth)
        , position relative
        , margin (px 1)
        ]
        [ Html.button
            [ Attrs.css
                [ Style.basicButton size
                , width (px buttonWidth)
                , height (px ((Style.noteHeight size * 2) + 2))
                , position absolute
                , top (px 0)
                , left (px 0)
                , hover [ color Colors.point1 ]
                , active [ Style.indent ]
                , cursor pointer
                ]
            , Events.onClick DeleteTrackerClicked
            ]
            [ Html.text "x" ]
        ]
    , Grid.column
        [ flex (int 0)
        , margin (px 1)
        ]
        [ trackerOptionsButton ]
    , Grid.column
        [ margin (px 1)
        , paddingLeft (px 5)
        ]
        [ Html.p
            [ Attrs.css
                [ Style.font size
                , lineHeight <| px <| Style.noteHeight size
                ]
            ]
            [ Html.text part.name ]
        ]
    ]



-- COLUMN OPTIONS --


voiceOptions : Part -> Style.Size -> List (Html Msg)
voiceOptions part size =
    List.range 0 (Part.voiceCount part - 1)
        |> List.map (voiceOption size)
        |> (::) (addVoiceZero size)


voiceOption : Style.Size -> Int -> Html Msg
voiceOption size i =
    let
        buttonWidth : Float
        buttonWidth =
            Style.noteWidth size / 2 - 1
    in
    Grid.column
        [ position relative
        , width (px (Style.noteWidth size))
        , margin (px 1)
        ]
        [ Html.button
            [ Attrs.css
                [ Style.basicButton size
                , width (px buttonWidth)
                , minHeight fitContent
                , active [ Style.indent ]
                , cursor pointer
                , hover [ color Colors.point1 ]
                , marginRight (px 2)
                ]
            , Events.onClick (DeleteVoiceClicked i)
            ]
            [ Html.text "x"
            ]
        , Html.button
            [ Attrs.css
                [ Style.basicButton size
                , width (px buttonWidth)
                , minHeight fitContent
                , active [ Style.indent ]
                , cursor pointer
                , hover [ color Colors.point1 ]
                ]
            , Events.onClick (AddVoiceClicked i)
            ]
            [ Html.text "+>"
            ]
        ]


addVoiceZero : Style.Size -> Html Msg
addVoiceZero size =
    Grid.column
        [ margin (px 1)
        , paddingLeft (px (Style.noteWidth size + 5))
        ]
        [ Html.button
            [ Attrs.css
                [ Style.basicButton size
                , width (px (Style.noteWidth size))
                , minHeight fitContent
                , active [ Style.indent ]
                , cursor pointer
                , hover [ color Colors.point1 ]
                ]
            , Events.onClick (AddVoiceClicked -1)
            ]
            [ Html.text "+>"
            ]
        ]



-- COLUMN NUMBERS --


voiceNumbers : Part -> Style.Size -> List (Html Msg)
voiceNumbers part size =
    let
        addBeatButton : Html Msg
        addBeatButton =
            Grid.column
                [ margin (px 1)
                , paddingRight (px (Style.noteWidth size + 2))
                ]
                [ Buttons.plus
                    AddBeatBelowClicked
                    [ width (pct 100) ]
                    size
                ]

        voiceNumber : Int -> Html Msg
        voiceNumber i =
            Grid.column
                [ width (px (Style.noteWidth size))
                , flex none
                , margin (px 1)
                ]
                [ Html.button
                    [ Attrs.css
                        [ Style.basicButton size
                        , width (px (Style.noteWidth size))
                        , minHeight fitContent
                        , margin (px 0)
                        , zIndex (int 1)
                        , Style.flush
                        ]
                    ]
                    [ Html.text (String.fromInt i) ]
                ]
    in
    List.range 0 (Part.voiceCount part - 1)
        |> List.map voiceNumber
        |> (::) addBeatButton
