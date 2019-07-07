module Ui.Tracker exposing
    ( Msg
    , update
    , view
    )

import Array
import Css exposing (..)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Index as Index exposing (Index)
import Data.Note exposing (Note)
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker exposing (Tracker)
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
import View.Button as Button



-- TYPES --


type Msg
    = BeatMsg (Index (Beat Encoding.None)) Beat.Msg
    | OptionsMsg Ui.Tracker.Options.Msg
    | OptionsClicked
    | DeleteTrackerClicked
    | AddBeatBelowClicked
    | DeleteVoiceClicked (Index (Note Encoding.None))
    | AddVoiceClicked (Index (Note Encoding.None))



-- UPDATE --


update : Index Tracker -> Index Part -> Msg -> Model -> ( Model, Cmd Msg )
update trackerIndex partIndex msg model =
    case msg of
        OptionsClicked ->
            case Model.getTrackersPart trackerIndex model of
                Just part ->
                    Model.mapTracker
                        trackerIndex
                        (Tracker.openOptions part.name)
                        model
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        OptionsMsg subMsg ->
            Ui.Tracker.Options.update
                trackerIndex
                subMsg
                model
                |> CmdUtil.withNoCmd

        BeatMsg beatIndex subMsg ->
            Beat.update trackerIndex partIndex beatIndex subMsg model
                |> CmdUtil.mapCmd (BeatMsg beatIndex)

        DeleteTrackerClicked ->
            model
                |> Model.removeTracker trackerIndex
                |> CmdUtil.withNoCmd

        AddBeatBelowClicked ->
            Model.mapPart
                partIndex
                Part.addBeatToBeginning
                model
                |> CmdUtil.withNoCmd

        AddVoiceClicked index ->
            Model.mapPart
                partIndex
                (Part.addVoice index)
                model
                |> CmdUtil.withNoCmd

        DeleteVoiceClicked index ->
            Model.mapPart
                partIndex
                (Part.removeVoice index)
                model
                |> CmdUtil.withNoCmd



-- VIEW --


type alias ViewParams =
    { trackerIndex : Index Tracker
    , tracker : Tracker
    , part : Part
    , partNames : List ( Index Part, String )
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


optionsContainerView : Tracker -> List ( Index Part, String ) -> Html Msg
optionsContainerView tracker partNames =
    case tracker.options of
        Just options ->
            Html.div
                [ Attrs.css
                    [ Style.dim
                    , width (pct 100)
                    , height (pct 100)
                    , position absolute
                    , zIndex (int 2)
                    ]
                ]
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


beatsView : Part -> Int -> Int -> Style.Size -> Index Tracker -> Html Msg
beatsView part majorMark minorMark size trackerIndex =
    let
        wrapBeat : ( Index (Beat Encoding.None), Beat Encoding.None ) -> Html Msg
        wrapBeat ( beatIndex, beat ) =
            Html.Styled.Lazy.lazy6
                Beat.view
                majorMark
                minorMark
                size
                trackerIndex
                beatIndex
                beat
                |> Html.map (BeatMsg beatIndex)
    in
    part.beats
        |> Index.toEntries
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
    in
    [ Grid.column
        [ flex none
        , flexBasis (px buttonWidth)
        , position relative
        , margin (px 1)
        ]
        [ Html.button
            [ Attrs.css
                [ Style.clickableButtonStyle size
                , width (px buttonWidth)
                , height (px ((Style.noteHeight size * 2) + 2))
                , position absolute
                , top (px 0)
                , left (px 0)
                ]
            , Events.onClick DeleteTrackerClicked
            ]
            [ Html.text "x" ]
        ]
    , Grid.column
        [ flex (int 0)
        , margin (px 1)
        ]
        [ Button.button OptionsClicked "options"
            |> Button.withWidth Button.doubleWidth
            |> Button.withSize size
            |> Button.toHtml
        ]
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
    part.beats
        |> Array.get 0
        |> Maybe.map (Beat.toIndexedList >> List.map Tuple.first)
        |> Maybe.withDefault []
        |> List.map (voiceOption size)
        |> (::) (addVoiceZero size)


voiceOption : Style.Size -> Index (Note Encoding.None) -> Html Msg
voiceOption size i =
    Grid.column
        [ position relative
        , width (px (Style.noteWidth size))
        , margin (px 1)
        ]
        [ Html.div
            [ Attrs.css
                [ marginRight (px 2)
                , displayFlex
                ]
            ]
            [ Button.button (DeleteVoiceClicked i) "x"
                |> Button.withWidth Button.halfWidth
                |> Button.withSize size
                |> Button.toHtml
            ]
        , Button.button (AddVoiceClicked i) "+>"
            |> Button.withWidth Button.halfWidth
            |> Button.withSize size
            |> Button.toHtml
        ]


addVoiceZero : Style.Size -> Html Msg
addVoiceZero size =
    Grid.column
        [ margin (px 1)
        , paddingLeft (px (Style.noteWidth size + 5))
        ]
        [ Button.button (AddVoiceClicked <| Index.previous Index.zero) "+>"
            |> Button.withWidth Button.singleWidth
            |> Button.withSize size
            |> Button.toHtml
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
                [ Button.button AddBeatBelowClicked "+v"
                    |> Button.withWidth Button.fullWidth
                    |> Button.withSize size
                    |> Button.toHtml
                ]

        voiceNumber : Int -> Html Msg
        voiceNumber i =
            Grid.column
                [ width (px (Style.noteWidth size))
                , flex none
                , margin (px 1)
                ]
                [ Html.p
                    [ Attrs.css
                        [ textAlign center
                        , Style.singleWidth size
                        , Style.font size
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
