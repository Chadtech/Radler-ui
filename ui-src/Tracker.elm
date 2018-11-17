module Tracker exposing
    ( Msg
    , update
    , view
    )

import Array exposing (Array)
import Beat
import Colors
import Css exposing (..)
import Data.Beat as Beat exposing (Beat)
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker
    exposing
        ( OptionsModel
        , Tracker
        )
import Html.Buttons as Buttons
import Html.Grid as Grid
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Keyed
import Html.Styled.Lazy
import Model exposing (Model)
import Note
import Return2 as R2
import Style
import Tracker.Options as Options
import Util



-- TYPES --


type alias Payload =
    { part : Part
    , majorMark : Int
    , minorMark : Int
    , size : Style.Size
    , options : Maybe OptionsPayload
    , trackerIndex : Int
    }


type alias OptionsPayload =
    { partNames : List ( Int, String )
    , majorMarkField : String
    , minorMarkField : String
    }


type Msg
    = NameFieldUpdated String
    | BeatMsg Int Beat.Msg
    | OptionsMsg Options.Msg
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
        NameFieldUpdated str ->
            Model.mapPart
                pi
                (Part.setName str)
                model
                |> R2.withNoCmd

        OptionsClicked ->
            Model.mapTracker
                ti
                Tracker.openOptions
                model
                |> R2.withNoCmd

        OptionsMsg subMsg ->
            Model.mapTracker
                ti
                (Options.update subMsg)
                model
                |> R2.withNoCmd

        BeatMsg bi subMsg ->
            Beat.update ti pi bi subMsg model
                |> R2.mapCmd (BeatMsg bi)

        DeleteTrackerClicked ->
            Model.removeTracker ti model
                |> R2.withNoCmd

        AddBeatBelowClicked ->
            Model.mapPart
                pi
                (Part.addBeat -1)
                model
                |> R2.withNoCmd

        AddVoiceClicked index ->
            Model.mapPart
                pi
                (Part.addVoice index)
                model
                |> R2.withNoCmd

        DeleteVoiceClicked index ->
            Model.mapPart
                pi
                (Part.removeVoice index)
                model
                |> R2.withNoCmd



-- VIEW --


view : Model -> Int -> Tracker -> Html Msg
view model trackerIndex tracker =
    case Array.get tracker.partIndex model.parts of
        Just part ->
            { part = part
            , majorMark = tracker.majorMark
            , minorMark = tracker.minorMark
            , size = tracker.size
            , options =
                Maybe.map
                    (toOptionsPayload model.parts)
                    tracker.options
            , trackerIndex = trackerIndex
            }
                |> fromPayload

        Nothing ->
            notFoundView


toOptionsPayload : Array Part -> OptionsModel -> OptionsPayload
toOptionsPayload parts { majorMarkField, minorMarkField } =
    { majorMarkField = majorMarkField
    , minorMarkField = minorMarkField
    , partNames =
        parts
            |> Array.toIndexedList
            |> List.map (Tuple.mapSecond .name)
    }


fromPayload : Payload -> Html Msg
fromPayload payload =
    Grid.container
        [ Style.card
        , flexDirection Css.column
        , height (calc (vh 100) minus (px 84))
        , overflow hidden
        , position relative
        ]
        (contentView payload)


contentView : Payload -> List (Html Msg)
contentView payload =
    [ optionsContainerView payload
    , Grid.row
        [ minHeight fitContent ]
        (trackerOptions payload)
    , Grid.row
        [ minHeight fitContent ]
        (voiceOptions payload)
    , Grid.row
        [ minHeight fitContent ]
        (voiceNumbers payload)
    , Grid.row
        []
        [ Html.Styled.Lazy.lazy5
            beatsView
            payload.part
            payload.majorMark
            payload.minorMark
            payload.size
            payload.trackerIndex
        ]
    ]


optionsContainerView : Payload -> Html Msg
optionsContainerView payload =
    case payload.options of
        Just options ->
            { parts = options.partNames
            , size = payload.size
            , majorMarkField = options.majorMarkField
            , minorMarkField = options.minorMarkField
            , majorMark = payload.majorMark
            , minorMark = payload.minorMark
            }
                |> Options.view
                |> List.singleton
                |> Html.div
                    [ Attrs.css [ optionsContainerStyle ] ]
                |> Html.map OptionsMsg

        Nothing ->
            Html.text ""


optionsContainerStyle : Style
optionsContainerStyle =
    [ backgroundColor (rgba 0 0 0 0.5)
    , width (pct 100)
    , height (pct 100)
    , position absolute
    , zIndex (int 2)
    ]
        |> Css.batch


beatsView : Part -> Int -> Int -> Style.Size -> Int -> Html Msg
beatsView part majorMark minorMark size ti =
    beatsContentView part majorMark minorMark size ti
        |> Grid.container [ overflow auto ]


beatsContentView : Part -> Int -> Int -> Style.Size -> Int -> List (Html Msg)
beatsContentView part majorMark minorMark size ti =
    part.beats
        |> Array.toIndexedList
        |> List.map (wrapBeat majorMark minorMark size ti)


wrapBeat : Int -> Int -> Style.Size -> Int -> ( Int, Beat ) -> Html Msg
wrapBeat majorMark minorMark size ti ( bi, beat ) =
    Html.Styled.Lazy.lazy6
        Beat.view
        majorMark
        minorMark
        size
        ti
        bi
        beat
        |> Html.map (BeatMsg bi)



-- TRACKER OPTIONS --


{-|

    The options on the very top of each tracker
    view, including the X button the close the
    tracker and the name of the part, which can
    be clicked to reveal options about this tracker

-}
trackerOptions : Payload -> List (Html Msg)
trackerOptions payload =
    let
        buttonWidth : Float
        buttonWidth =
            Style.noteWidth payload.size + 3
    in
    [ Grid.column
        [ flex none
        , flexBasis (px buttonWidth)
        , position relative
        , margin (px 1)
        ]
        [ Html.button
            [ Attrs.css
                [ Style.basicButton payload.size
                , width (px buttonWidth)
                , height (px ((Style.noteHeight payload.size * 2) + 2))
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
        [ trackerOptionsButton payload.size ]
    , Grid.column
        [ margin (px 1) ]
        [ partNameField payload.size payload.part ]
    ]



-- COLUMN OPTIONS --


voiceOptions : Payload -> List (Html Msg)
voiceOptions { part, size } =
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


voiceNumbers : Payload -> List (Html Msg)
voiceNumbers { part, size } =
    List.range 0 (Part.voiceCount part - 1)
        |> List.map (voiceNumber size)
        |> (::) (addBeatButton size)


addBeatButton : Style.Size -> Html Msg
addBeatButton size =
    Grid.column
        [ margin (px 1)
        , paddingLeft (px ((Style.noteWidth size / 2) + 3))
        , paddingRight (px (Style.noteWidth size + 2))
        ]
        [ Buttons.plus
            AddBeatBelowClicked
            []
            size
        ]


voiceNumber : Style.Size -> Int -> Html Msg
voiceNumber size i =
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


trackerOptionsButton : Style.Size -> Html Msg
trackerOptionsButton size =
    Html.button
        [ Attrs.css
            [ Style.basicButton size
            , width (px ((Style.noteWidth size * 2) + 2))
            ]
        , Events.onClick OptionsClicked
        ]
        [ Html.text "options" ]


partNameField : Style.Size -> Part -> Html Msg
partNameField size part =
    Html.input
        [ Attrs.css [ partNameStyle size ]
        , Attrs.value part.name
        , Attrs.spellcheck False
        , Events.onInput NameFieldUpdated
        ]
        []


partNameStyle : Style.Size -> Style
partNameStyle size =
    [ Style.font size
    , color Colors.point0
    , width (pct 100)
    , Style.fontSmoothingNone
    ]
        |> Css.batch



-- NOT FOUND VIEW --


notFoundView : Html Msg
notFoundView =
    Html.div
        [ Attrs.css [ Style.card ] ]
        [ Html.p
            [ Attrs.css
                [ Style.hfnss
                , whiteSpace noWrap
                , margin (px 4)
                ]
            ]
            [ Html.text "Error : Part not found" ]
        ]
