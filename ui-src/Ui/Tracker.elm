module Ui.Tracker exposing
    ( Msg
    , update
    , view
    )

import Array
import Css exposing (..)
import Data.Beat as BeatData exposing (Beat)
import Data.Encoding as Encoding
import Data.Index as Index exposing (Index)
import Data.Note exposing (Note)
import Data.Part as Part exposing (Part)
import Data.Size as Size exposing (Size)
import Data.Tracker as Tracker exposing (Tracker)
import Data.Tracker.Collapse exposing (Collapse)
import Data.Width as Width
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Lazy
import Model exposing (Model)
import Style
import Ui.Beat as Beat
import Ui.Tracker.Options
import Util.Cmd as CmdUtil
import View.Button as Button
import View.Card as Card
import View.Text as Text



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
            Model.mapTracker
                trackerIndex
                Tracker.openOptions
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
    Card.config
        [ flexDirection Css.column
        , height (calc (vh 100) minus (px 95))
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
        , Html.Styled.Lazy.lazy6
            beatsView
            part
            tracker.majorMark
            tracker.minorMark
            tracker.size
            tracker.collapse
            (Index.toInt trackerIndex)
        ]


optionsContainerView : Tracker -> List ( Index Part, String ) -> Html Msg
optionsContainerView tracker partNames =
    case tracker.options of
        Just optionsModel ->
            Grid.box
                [ Style.dim
                , width (pct 100)
                , height (pct 100)
                , position absolute
                , zIndex (int 2)
                ]
                [ Ui.Tracker.Options.view
                    { parts = partNames
                    , size = tracker.size
                    , majorMark = tracker.majorMark
                    , minorMark = tracker.minorMark
                    , collapse = tracker.collapse
                    , model = optionsModel
                    }
                ]
                |> Html.map OptionsMsg

        Nothing ->
            Html.text ""


beatsView : Part -> Int -> Int -> Size -> Collapse -> Int -> Html Msg
beatsView part majorMark minorMark size collapse trackerIndex =
    let
        wrapBeat : ( Index (Beat Encoding.None), Beat Encoding.None ) -> ( String, Html Msg )
        wrapBeat ( beatIndex, beat ) =
            ( Index.toString beatIndex
            , Html.Styled.Lazy.lazy7
                Beat.view
                majorMark
                minorMark
                size
                collapse
                trackerIndex
                (Index.toInt beatIndex)
                beat
                |> Html.map (BeatMsg beatIndex)
            )
    in
    Grid.row
        []
        [ part.beats
            |> Index.toEntries
            |> List.map wrapBeat
            |> Grid.keyedColumn
                [ overflow auto
                , display block
                ]
        ]



-- TRACKER OPTIONS --


{-|

    The options on the very top of each tracker
    view, including the X button the close the
    tracker and the name of the part, which can
    be clicked to reveal options about this tracker

-}
trackerOptionsRow : Part -> Size -> List (Grid.Column Msg)
trackerOptionsRow part size =
    [ Grid.column
        [ flex none
        , flexBasis (px <| Size.toUnitWidth size + 3)
        , position relative
        , margin (px 1)
        , Style.height size
        ]
        [ Button.config DeleteTrackerClicked "x"
            |> Button.withWidth Width.full
            |> Button.withSize size
            |> Button.makeTallerBy (Size.toUnitHeight size + 2)
            |> Button.toHtml
        ]
    , Grid.column
        [ Grid.columnShrink
        , margin (px 1)
        ]
        [ Button.config OptionsClicked "options"
            |> Button.withWidth Width.double
            |> Button.withSize size
            |> Button.toHtml
        ]
    , Grid.column
        [ Style.basicSpacing
        , Style.leftPadding
        ]
        [ Text.withStyles
            [ Style.font size
            , lineHeight <| px <| Size.toUnitHeight size
            ]
            part.name
        ]
    ]



-- COLUMN OPTIONS --


voiceOptions : Part -> Size -> List (Grid.Column Msg)
voiceOptions part size =
    part.beats
        |> Array.get 0
        |> Maybe.map (BeatData.toIndexedList >> List.map Tuple.first)
        |> Maybe.withDefault []
        |> List.map (voiceOption size)
        |> (::) (addVoiceZero size)


voiceOption : Size -> Index (Note Encoding.None) -> Grid.Column Msg
voiceOption size i =
    Grid.column
        [ position relative
        , Style.singleWidth size
        , margin (px 1)
        ]
        [ Grid.box
            [ marginRight (px 2)
            , displayFlex
            ]
            [ Button.config (DeleteVoiceClicked i) "x"
                |> Button.withWidth Width.half
                |> Button.withSize size
                |> Button.toHtml
            ]
        , Button.config (AddVoiceClicked i) "+>"
            |> Button.withWidth Width.half
            |> Button.withSize size
            |> Button.toHtml
        ]


addVoiceZero : Size -> Grid.Column Msg
addVoiceZero size =
    Grid.column
        [ margin (px 1)
        , paddingLeft (px (Size.toUnitWidth size + 5))
        ]
        [ Button.config (AddVoiceClicked <| Index.previous Index.zero) "+>"
            |> Button.withWidth Width.single
            |> Button.withSize size
            |> Button.toHtml
        ]



-- COLUMN NUMBERS --


voiceNumbers : Part -> Size -> List (Grid.Column Msg)
voiceNumbers part size =
    let
        addBeatButton : Grid.Column Msg
        addBeatButton =
            Grid.column
                [ margin (px 1)
                , paddingRight (px (Size.toUnitWidth size + 2))
                ]
                [ Button.config AddBeatBelowClicked "+v"
                    |> Button.withWidth Width.full
                    |> Button.withSize size
                    |> Button.toHtml
                ]

        voiceNumber : Int -> Grid.Column Msg
        voiceNumber i =
            Grid.column
                [ Style.singleWidth size
                , flex none
                , margin (px 1)
                ]
                [ Text.withStyles
                    [ textAlign center
                    , Style.singleWidth size
                    , Style.font size
                    , zIndex (int 1)
                    , Style.flush
                    ]
                    (String.fromInt i)
                ]
    in
    List.range 0 (Part.voiceCount part - 1)
        |> List.map voiceNumber
        |> (::) addBeatButton
