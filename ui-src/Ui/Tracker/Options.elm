module Ui.Tracker.Options exposing
    ( Msg(..)
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Index exposing (Index)
import Data.Part exposing (Part)
import Data.Size as Size exposing (Size)
import Data.Tracker as Tracker exposing (Tracker)
import Data.Tracker.Collapse as Collapse exposing (Collapse)
import Data.Tracker.Options as TrackerOptions
import Data.Width as Width
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Model exposing (Model)
import Style
import View.Button as Button
import View.Card as Card
import View.Checkbox as Checkbox
import View.Input as Input
import View.Text as Text



-- TYPES --


type Msg
    = PartClicked (Index Part)
    | BackClicked
    | SmallClicked
    | BigClicked
    | MajorMarkFieldUpdated String
    | MinorMarkFieldUpdated String
    | CollapseClicked Collapse
    | CollapseEveryChanged String



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
            Tracker.setSize Size.small

        BigClicked ->
            Tracker.setSize Size.big

        MajorMarkFieldUpdated "" ->
            Tracker.setMajorMark 0

        MajorMarkFieldUpdated field ->
            case String.toInt field of
                Just newMajorMark ->
                    Tracker.setMajorMark newMajorMark

                Nothing ->
                    identity

        MinorMarkFieldUpdated "" ->
            Tracker.setMinorMark 0

        MinorMarkFieldUpdated field ->
            case String.toInt field of
                Just newMinorMark ->
                    Tracker.setMinorMark newMinorMark

                Nothing ->
                    identity

        CollapseClicked collapse ->
            Tracker.setCollapse collapse

        CollapseEveryChanged "" ->
            setCollapseForEvery 0

        CollapseEveryChanged str ->
            case String.toInt str of
                Just every ->
                    setCollapseForEvery every

                Nothing ->
                    identity


setCollapseForEvery : Int -> Tracker -> Tracker
setCollapseForEvery every =
    Tracker.setCollapse (Collapse.every every)
        >> Tracker.mapOptions (TrackerOptions.setEveryField every)



-- VIEW --


type alias ViewParams =
    { parts : List ( Index Part, String )
    , size : Size
    , majorMark : Int
    , minorMark : Int
    , collapse : Collapse
    , model : TrackerOptions.Model
    }


view : ViewParams -> Html Msg
view params =
    Card.config
        [ transform (translate2 (pct -50) (pct -50))
        , position absolute
        , top (pct 50)
        , left (pct 50)
        , width (px 375)
        ]
        [ Grid.box
            [ margin (px 0)
            , width (pct 100)
            ]
            [ row
                [ Grid.column
                    []
                    [ Text.withStyles
                        [ padding (px 5)
                        , backgroundColor Colors.point0
                        , color Colors.ignorable2
                        , width (pct 100)
                        ]
                        "tracker options"
                    ]
                ]
            , row
                [ Grid.column
                    []
                    [ Text.fromString "switch to part.." ]
                ]
            , row
                [ partOptionsContainer params ]
            , row
                [ markLabel "major mark"
                , markField
                    params.majorMark
                    MajorMarkFieldUpdated
                ]
            , row
                [ markLabel "minor mark"
                , markField
                    params.minorMark
                    MinorMarkFieldUpdated
                ]
            , Grid.row
                [ margin (px 5)
                , flexDirection Css.column
                ]
                [ Grid.column
                    [ flexDirection Css.column ]
                    (collapseOptions params)
                ]
            , row
                [ smallViewButton params.size
                , bigViewButton params.size
                ]
            , row
                [ Grid.column
                    []
                    [ Button.config BackClicked "back"
                        |> Button.withWidth Width.full
                        |> Button.toHtml
                    ]
                ]
            ]
        ]


collapseOptions : ViewParams -> List (Html Msg)
collapseOptions { model, collapse } =
    let
        collapseOptionBody : Collapse -> List (Grid.Column Msg)
        collapseOptionBody thisCollapse =
            let
                collapseCheckbox : Grid.Column Msg
                collapseCheckbox =
                    Grid.column
                        [ marginRight (px 5)
                        , flex (int 0)
                        ]
                        [ Checkbox.checkbox
                            (CollapseClicked thisCollapse)
                            (Collapse.areSame collapse thisCollapse)
                            |> Checkbox.toHtml
                        ]

                collapseLabelHtml : List Style -> Grid.Column Msg
                collapseLabelHtml columnStyles =
                    Grid.column
                        columnStyles
                        [ Text.withStyles
                            [ lineHeight (px 30) ]
                            (Collapse.toLabel thisCollapse)
                        ]
            in
            case thisCollapse of
                Collapse.Every everyAmount ->
                    [ collapseCheckbox
                    , collapseLabelHtml
                        [ Grid.columnShrink ]
                    , Grid.column
                        [ paddingLeft (px 5) ]
                        [ Input.config
                            CollapseEveryChanged
                            (String.fromInt everyAmount)
                            |> Input.makeTallerBy 4
                            |> Input.toHtml
                        ]
                    ]

                _ ->
                    [ collapseCheckbox
                    , collapseLabelHtml []
                    ]

        collapseOption : Collapse -> Html Msg
        collapseOption thisCollapse =
            Grid.row
                [ margin2 (px 5) (px 0) ]
                (collapseOptionBody thisCollapse)
    in
    Text.fromString "collapse.."
        :: List.map
            collapseOption
            (Collapse.all model.collapseEveryField)


row : List (Grid.Column Msg) -> Html Msg
row =
    Grid.row [ margin (px 5) ]


markLabel : String -> Grid.Column Msg
markLabel labelText =
    Grid.column
        []
        [ Text.withStyles
            [ lineHeight (px 26) ]
            labelText
        ]


markField : Int -> (String -> Msg) -> Grid.Column Msg
markField mark msgCtor =
    Grid.column
        [ paddingLeft (px 5) ]
        [ Input.config msgCtor (String.fromInt mark)
            |> Input.withWidth Width.full
            |> Input.toHtml
        ]


smallViewButton : Size -> Grid.Column Msg
smallViewButton size =
    Grid.column
        []
        [ Button.config SmallClicked "small"
            |> Button.indent (size == Size.small)
            |> Button.withWidth Width.full
            |> Button.toHtml
        ]


bigViewButton : Size -> Grid.Column Msg
bigViewButton size =
    Grid.column
        [ paddingLeft (px 5) ]
        [ Button.config BigClicked "big"
            |> Button.indent (size == Size.big)
            |> Button.withWidth Width.full
            |> Button.toHtml
        ]


partOptionsContainer : ViewParams -> Grid.Column Msg
partOptionsContainer params =
    Grid.column
        [ Style.indent
        , backgroundColor Colors.background3
        , width (pct 100)
        , maxHeight (px 200)
        , overflow auto
        ]
        [ Grid.box
            [ width (pct 100) ]
            (List.map partOptionView params.parts)
        ]


partOptionView : ( Index Part, String ) -> Html Msg
partOptionView ( index, name ) =
    Grid.row
        [ Style.basicSpacing ]
        [ Grid.column
            []
            [ Text.config
                { styles =
                    [ marginLeft (px 10)
                    , cursor pointer
                    , width (pct 100)
                    , hover
                        [ backgroundColor Colors.background4
                        , color Colors.point1
                        ]
                    ]
                , options =
                    [ Text.onClick (PartClicked index) ]
                , value = name
                }
            ]
        ]
