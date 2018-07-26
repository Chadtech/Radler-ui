module Html.Tracker
    exposing
        ( Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Colors
import Css exposing (..)
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker exposing (Tracker)
import Html.Beat as Beat
import Html.Buttons as Buttons
import Html.Custom exposing (p)
import Html.Details as Details
import Html.Grid as Grid
import Html.Note
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        )
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, onMouseLeave)
import Html.Styled.Keyed
import Html.Styled.Lazy
import Model exposing (Model)
import Return2 as R2
import Set exposing (Set)
import Style
import Util


-- TYPES --


type alias Payload =
    { part : Part
    , majorMark : Int
    , minorMark : Int
    , size : Style.Size
    , partDetails : Maybe (List ( Int, String ))
    , toggledColumns : Set Int
    , trackerIndex : Int
    }


type Msg
    = BeatMsg Int Beat.Msg
    | DetailsMsg Details.Msg
    | NameClicked
    | DeleteTrackerClicked
    | AddBeatBelowClicked
    | ColumnNumberClicked Int
    | ColumnNumberExited Int
    | DeleteColumnClicked Int
    | AddColumnClicked Int



-- UPDATE --


update : Int -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update ti si msg model =
    case msg of
        NameClicked ->
            Model.mapTracker
                ti
                Tracker.openDetails
                model
                |> R2.withNoCmd

        DetailsMsg (Details.NameFieldUpdated str) ->
            Model.mapPart
                si
                (Part.setName str)
                model
                |> R2.withNoCmd

        DetailsMsg (Details.PartClicked index) ->
            Model.mapTracker
                ti
                (Tracker.setPartIndex index)
                model
                |> R2.withNoCmd

        DetailsMsg Details.BackClicked ->
            Model.mapTracker
                ti
                Tracker.closeDetails
                model
                |> R2.withNoCmd

        DetailsMsg Details.SmallClicked ->
            Model.mapTracker
                ti
                (Tracker.setSize Style.Small)
                model
                |> R2.withNoCmd

        DetailsMsg Details.BigClicked ->
            Model.mapTracker
                ti
                (Tracker.setSize Style.Big)
                model
                |> R2.withNoCmd

        DetailsMsg (Details.MajorMarkFieldUpdated field) ->
            case String.toInt field of
                Just majorMark ->
                    Model.mapTracker
                        ti
                        (Tracker.setMajorMark majorMark)
                        model
                        |> R2.withNoCmd

                Nothing ->
                    model
                        |> R2.withNoCmd

        DetailsMsg (Details.MinorMarkFieldUpdated field) ->
            case String.toInt field of
                Just minorMark ->
                    Model.mapTracker
                        ti
                        (Tracker.setMinorMark minorMark)
                        model
                        |> R2.withNoCmd

                Nothing ->
                    model
                        |> R2.withNoCmd

        BeatMsg bi subMsg ->
            Beat.update ti si bi subMsg model
                |> R2.mapCmd (BeatMsg bi)

        DeleteTrackerClicked ->
            Model.removeTracker ti model
                |> R2.withNoCmd

        AddBeatBelowClicked ->
            Model.mapPart
                si
                (Part.addBeat -1)
                model
                |> R2.withNoCmd

        ColumnNumberClicked index ->
            Model.mapTracker
                ti
                (Tracker.addToggledColumn index)
                model
                |> R2.withNoCmd

        ColumnNumberExited index ->
            Model.mapTracker
                ti
                (Tracker.removeToggledColumn index)
                model
                |> R2.withNoCmd

        AddColumnClicked index ->
            Model.mapPart
                si
                (Part.addColumn index)
                model
                |> R2.withNoCmd

        DeleteColumnClicked index ->
            model
                |> Model.mapPart si (Part.removeColumn index)
                |> Model.mapTracker ti Tracker.clearToggledColumns
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
            , partDetails =
                if tracker.partDetails then
                    model.parts
                        |> Array.toIndexedList
                        |> List.map (Tuple.mapSecond .name)
                        |> Just
                else
                    Nothing
            , toggledColumns = tracker.toggledColumns
            , trackerIndex = trackerIndex
            }
                |> fromPayload

        Nothing ->
            notFoundView


fromPayload : Payload -> Html Msg
fromPayload payload =
    Grid.container
        [ Style.card
        , flexDirection Css.column
        , height (calc (vh 100) minus (px 78))
        , overflow hidden
        , position relative
        ]
        (contentView payload)


contentView : Payload -> List (Html Msg)
contentView payload =
    [ detailsContainerView payload
    , Grid.row
        [ minHeight fitContent ]
        (trackerOptions payload)
    , Grid.row
        [ minHeight fitContent
        , marginBottom (px 1)
        ]
        (columnOptions payload)
    , Grid.row
        [ minHeight fitContent
        , marginBottom (px 1)
        ]
        (columnNumbers payload)
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


detailsContainerView : Payload -> Html Msg
detailsContainerView payload =
    case payload.partDetails of
        Just partNames ->
            { partNameField = payload.part.name
            , parts = partNames
            , size = payload.size
            , majorMark = payload.majorMark
            , minorMark = payload.minorMark
            }
                |> Details.view
                |> List.singleton
                |> div [ css [ detailsContainerStyle ] ]
                |> Html.map DetailsMsg

        Nothing ->
            Html.text ""


detailsContainerStyle : Style
detailsContainerStyle =
    [ backgroundColor (rgba 0 0 0 0.5)
    , width (pct 100)
    , height (pct 100)
    , position absolute
    , zIndex (int 2)
    ]
        |> Css.batch


beatsView : Part -> Int -> Int -> Style.Size -> Int -> Html Msg
beatsView part majorMark minorMark size ti =
    Grid.container
        [ overflow auto ]
        (beatsContentView part majorMark minorMark size ti)


beatsContentView : Part -> Int -> Int -> Style.Size -> Int -> List (Html Msg)
beatsContentView part majorMark minorMark size ti =
    part.beats
        |> Array.toIndexedList
        |> List.map (wrapBeat majorMark minorMark size ti)


wrapBeat : Int -> Int -> Style.Size -> Int -> ( Int, Array String ) -> Html Msg
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


trackerOptions : Payload -> List (Html Msg)
trackerOptions payload =
    [ Grid.column
        [ flex none
        , flexBasis (px (Style.noteWidth payload.size + 4))
        , position relative
        ]
        [ button
            [ css
                [ Style.basicButton payload.size
                , width (px (Style.noteWidth payload.size + 2))
                , height (px ((Style.noteHeight payload.size * 2) + 2))
                , position absolute
                , top (px 0)
                , left (px 0)
                , hover [ color Colors.point1 ]
                , active [ Style.indent ]
                ]
            , onClick DeleteTrackerClicked
            ]
            [ Html.text "x" ]
        ]
    , Grid.column
        []
        [ partNameView payload.part payload.size ]
    ]



-- COLUMN OPTIONS --


columnOptions : Payload -> List (Html Msg)
columnOptions { part, toggledColumns, size } =
    List.range 0 (Part.columnCount part - 1)
        |> List.map (columnOption toggledColumns size)
        |> (::) (addColumnZero size)


columnOption : Set Int -> Style.Size -> Int -> Html Msg
columnOption toggledColumns size i =
    [ div
        [ css
            [ position absolute
            , top (px 0)
            , left (px 0)
            , width (px (Style.noteWidth size + 6))
            , minHeight fitContent
            , displayFlex
            ]
        ]
        [ button
            [ css
                [ Style.basicButton size
                , width (px (Style.noteWidth size / 2 + 2))
                , minHeight fitContent
                , active [ Style.indent ]
                , cursor pointer
                , hover [ color Colors.point1 ]
                ]
            , onClick (DeleteColumnClicked i)
            ]
            [ Html.text "x"
            ]
        , button
            [ css
                [ Style.basicButton size
                , width (px (Style.noteWidth size / 2 + 2))
                , minHeight fitContent
                , active [ Style.indent ]
                , cursor pointer
                , hover [ color Colors.point1 ]
                , marginRight (px 0)
                ]
            , onClick (AddColumnClicked i)
            ]
            [ Html.text "+>"
            ]
        ]
    ]
        |> Grid.column [ position relative ]


addColumnZero : Style.Size -> Html Msg
addColumnZero size =
    Grid.column
        []
        [ button
            [ css
                [ Style.basicButton size
                , width (px (Style.noteWidth size))
                , minHeight fitContent
                , active [ Style.indent ]
                , marginLeft (px (Style.noteWidth size + 5))
                , cursor pointer
                , hover [ color Colors.point1 ]
                ]
            , onClick (AddColumnClicked -1)
            ]
            [ Html.text "+>"
            ]
        ]



-- COLUMN NUMBERS --


columnNumbers : Payload -> List (Html Msg)
columnNumbers { part, toggledColumns, size } =
    List.range 0 (Part.columnCount part - 1)
        |> List.map (columnNumber toggledColumns size)
        |> (::) (addBeatButton size)


addBeatButton : Style.Size -> Html Msg
addBeatButton size =
    Buttons.plus
        AddBeatBelowClicked
        [ margin (px 0)
        , marginRight (px (Style.noteWidth size + 4))
        , marginLeft (px ((Style.noteWidth size / 2) + 3))
        ]
        size


columnNumber : Set Int -> Style.Size -> Int -> Html Msg
columnNumber toggledColumns size i =
    [ button
        [ css
            [ Style.basicButton size
            , width (px (Style.noteWidth size + 6))
            , minHeight fitContent
            , position absolute
            , margin (px 0)
            , zIndex (int 1)
            ]
        ]
        [ Html.text (String.fromInt i) ]
    ]
        |> Grid.column [ position relative ]


partNameView : Part -> Style.Size -> Html Msg
partNameView part size =
    button
        [ css [ partNameStyle size ]
        , onClick NameClicked
        ]
        [ Html.text part.name ]


partNameStyle : Style.Size -> Style
partNameStyle size =
    [ Style.basicButton size
    , width (pct 100)
    , hover [ color Colors.point1 ]
    , active [ Style.indent ]
    ]
        |> Css.batch



-- NOT FOUND VIEW --


notFoundView : Html Msg
notFoundView =
    div
        [ css [ Style.card ] ]
        [ p
            [ css
                [ Style.basicP
                , Style.hfnss
                , whiteSpace noWrap
                , margin (px 4)
                ]
            ]
            [ Html.text "Error : Part not found" ]
        ]