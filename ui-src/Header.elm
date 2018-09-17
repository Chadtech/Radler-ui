module Header exposing
    ( Msg
    , update
    , view
    )

import Array
import Colors
import Css exposing (..)
import Data.Package as Package
import Data.Part as Part
import Data.Tracker as Tracker
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Html
        , button
        , input
        , p
        )
import Html.Styled.Attributes as Attrs
import Html.Styled.Events
    exposing
        ( onClick
        , onInput
        )
import Model exposing (Model, Page)
import Return2 as R2
import Style



-- TYPES --


type Msg
    = PageClicked Page
    | NewSheetClicked
    | NewTrackerClicked
    | PlayClicked
    | OpenClicked
    | SaveClicked
    | PlayFromFieldUpdated String
    | PlayForFieldUpdated String



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageClicked page ->
            { model | page = page }
                |> R2.withNoCmd

        NewSheetClicked ->
            { model
                | parts =
                    Array.push Part.empty model.parts
            }
                |> R2.withNoCmd

        NewTrackerClicked ->
            { model
                | trackers =
                    Array.push
                        (Tracker.init Style.Small 0)
                        model.trackers
            }
                |> R2.withNoCmd

        PlayClicked ->
            model
                |> R2.withNoCmd

        OpenClicked ->
            model
                |> R2.withNoCmd

        SaveClicked ->
            let
                saveCmds =
                    [ Package.saveToDisk
                        model.package
                    , Model.saveParts
                        model
                    ]
            in
            case Model.saveScore model of
                Ok scoreCmd ->
                    scoreCmd
                        :: saveCmds
                        |> Cmd.batch
                        |> R2.withModel model

                Err newModel ->
                    newModel
                        |> R2.withCmds saveCmds

        PlayFromFieldUpdated str ->
            model
                |> Model.setPlayFrom str
                |> R2.withNoCmd

        PlayForFieldUpdated str ->
            model
                |> Model.setPlayFor str
                |> R2.withNoCmd



-- VIEW --


view : Model -> Html Msg
view model =
    Grid.row
        [ Style.card
        , displayFlex
        , minHeight minContent
        ]
        [ Grid.column
            [ flex (int 0) ]
            [ saveButton ]
        , Grid.column
            [ flex (int 0) ]
            [ playButton ]
        , Grid.column
            [ flex (int 0)
            , margin2 (px 0) (px 10)
            ]
            [ fromText ]
        , Grid.column
            [ flex (int 0) ]
            [ playFromField model.playFromBeatField ]
        , Grid.column
            [ flex (int 0)
            , margin2 (px 0) (px 10)
            ]
            [ forText ]
        , Grid.column
            [ flex (int 0) ]
            [ playForField model.playForBeatsField ]
        , Grid.column
            [ flex (int 0)
            , marginLeft (px 15)
            ]
            [ trackersButton model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ packageButton model.page ]
        , Grid.column
            [ marginLeft (px 10)
            , flex (int 0)
            ]
            [ newSheetButton ]
        , Grid.column
            [ flex (int 0) ]
            [ newTrackerButton ]
        ]


fromText : Html Msg
fromText =
    p
        [ Attrs.css
            [ Style.basicP
            , Style.hfnss
            , lineHeight (px 32)
            ]
        ]
        [ Html.text "from" ]


forText : Html Msg
forText =
    p
        [ Attrs.css
            [ Style.basicP
            , Style.hfnss
            , lineHeight (px 32)
            ]
        ]
        [ Html.text "for" ]


playForField : String -> Html Msg
playForField playForBeats =
    input
        [ Attrs.css
            [ Style.basicInput
            , margin2 (px 1) (px 0)
            , Style.hfnss
            , color Colors.point0
            , Style.fontSmoothingNone
            , width (px ((Style.noteWidth Style.Big * 1.5) / 2))
            ]
        , Attrs.value playForBeats
        , Attrs.spellcheck False
        , onInput PlayForFieldUpdated
        ]
        []


playFromField : String -> Html Msg
playFromField playFromBeat =
    input
        [ Attrs.css
            [ Style.basicInput
            , margin2 (px 1) (px 0)
            , Style.hfnss
            , color Colors.point0
            , Style.fontSmoothingNone
            , width (px ((Style.noteWidth Style.Big * 1.5) / 2))
            ]
        , Attrs.value playFromBeat
        , Attrs.spellcheck False
        , onInput PlayFromFieldUpdated
        ]
        []


playButton : Html Msg
playButton =
    button
        [ Attrs.css [ buttonStyle ]
        , onClick PlayClicked
        ]
        [ Html.text "play" ]


openButton : Html Msg
openButton =
    button
        [ Attrs.css [ buttonStyle ]
        , onClick OpenClicked
        ]
        [ Html.text "open" ]


saveButton : Html Msg
saveButton =
    button
        [ Attrs.css [ buttonStyle ]
        , onClick SaveClicked
        ]
        [ Html.text "save" ]


trackersButton : Page -> Html Msg
trackersButton page =
    button
        [ Attrs.css
            [ buttonStyle
            , dent page Model.Trackers
            ]
        , onClick (PageClicked Model.Trackers)
        ]
        [ Html.text "trackers" ]


packageButton : Page -> Html Msg
packageButton page =
    button
        [ Attrs.css
            [ buttonStyle
            , dent page Model.Package
            ]
        , onClick (PageClicked Model.Package)
        ]
        [ Html.text "package" ]


newSheetButton : Html Msg
newSheetButton =
    button
        [ Attrs.css [ buttonStyle ]
        , onClick NewSheetClicked
        ]
        [ Html.text "new part" ]


newTrackerButton : Html Msg
newTrackerButton =
    button
        [ Attrs.css
            [ buttonStyle
            , width (px (Style.noteWidth Style.Big * 2))
            ]
        , onClick NewTrackerClicked
        ]
        [ Html.text "new tracker" ]


buttonStyle : Style
buttonStyle =
    [ Style.hfnss
    , margin (px 1)
    , width (px (Style.noteWidth Style.Big * 1.5))
    , height (px (Style.noteHeight Style.Big + 4))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , outline none
    , active [ Style.indent ]
    , Style.outdent
    ]
        |> Css.batch


dent : Page -> Page -> Style
dent currentPage thisPage =
    if currentPage == thisPage then
        Style.indent

    else
        Style.outdent
