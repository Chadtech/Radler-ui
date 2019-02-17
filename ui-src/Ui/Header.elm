module Ui.Header exposing
    ( Msg
    , update
    , view
    )

import Array
import BackendStatus
import Colors
import Css exposing (..)
import Data.Error as Error
import Data.Modal as Modal
import Data.Package as Package
import Data.Part as Part
import Data.Tracker as Tracker
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Json.Decode as D
import Model exposing (Model, Page)
import Return2 as R2
import Score
import Style



-- TYPES --


type Msg
    = PageClicked Page
    | NewPartClicked
    | NewTrackerClicked
    | PlayClicked
    | OpenClicked
    | SaveClicked
    | BuildClicked
    | PlayFromFieldUpdated String
    | PlayForFieldUpdated String
    | PlaySent (Result Http.Error ())



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageClicked page ->
            { model | page = page }
                |> R2.withNoCmd

        NewPartClicked ->
            model
                |> Model.addNewPart
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
            case Model.score model of
                Ok scoreStr ->
                    scoreStr
                        |> Play
                        |> sendHttp model
                        |> R2.withModel
                            (Model.setBackendStatusWorking model)

                Err newModel ->
                    newModel
                        |> R2.withNoCmd

        OpenClicked ->
            model
                |> R2.withNoCmd

        SaveClicked ->
            [ Package.saveToDisk
                model.package
            , Model.saveParts
                model
            ]
                |> Cmd.batch
                |> R2.withModel model

        BuildClicked ->
            model
                |> Model.setModal Modal.initBuild
                |> R2.withNoCmd

        PlayFromFieldUpdated str ->
            model
                |> Model.setPlayFrom str
                |> R2.withNoCmd

        PlayForFieldUpdated str ->
            model
                |> Model.setPlayFor str
                |> R2.withNoCmd

        PlaySent (Ok ()) ->
            model
                |> Model.setBackendStatusIdle
                |> R2.withNoCmd

        PlaySent (Err err) ->
            model
                |> playFailed err
                |> Model.setBackendStatusIdle
                |> R2.withNoCmd


playFailed : Http.Error -> Model -> Model
playFailed error =
    error
        |> Score.errorToString
        |> Error.BackendHadProblemWithScore
        |> Model.setError



-- HTTP --


type Call
    = Play String


sendHttp : Model -> Call -> Cmd Msg
sendHttp model call =
    case call of
        Play score ->
            Score.sendHttp
                { model = model
                , path = "play"
                , score = score
                , msgCtor = PlaySent
                }



-- VIEW --


view : Model -> Html Msg
view model =
    Grid.container
        [ Style.card
        , displayFlex
        , minHeight minContent
        , flexDirection column
        ]
        [ playbackButtons model
        , uiButtons model
        ]


playbackButtons : Model -> Html Msg
playbackButtons model =
    Grid.row
        []
        [ Grid.column
            [ flex (int 0) ]
            [ saveButton ]
        , Grid.column
            [ flex (int 0) ]
            [ playButton ]
        , Grid.column
            [ flex (int 0) ]
            [ fromText ]
        , Grid.column
            [ flex (int 0) ]
            [ playFromField model.playFromBeatField ]
        , Grid.column
            [ flex (int 0) ]
            [ forText ]
        , Grid.column
            [ flex (int 0) ]
            [ playForField model.playForBeatsField ]
        , Grid.column
            [ flex (int 0) ]
            [ buildButton ]
        , Grid.column
            [ alignItems flexEnd
            , flexDirection column
            ]
            [ BackendStatus.view model.backendStatus ]
        ]


uiButtons : Model -> Html Msg
uiButtons model =
    Grid.row
        []
        [ Grid.column
            [ flex (int 0)
            , singleWidth
            ]
            [ pageText ]
        , Grid.column
            [ flex (int 0) ]
            [ trackersButton model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ packageButton model.page ]
        , horizontalSeparator
        , Grid.column
            [ flex (int 0) ]
            [ newPartButton ]
        , Grid.column
            [ flex (int 0) ]
            [ newTrackerButton ]
        ]


pageText : Html Msg
pageText =
    Html.p
        [ Attrs.css [ textStyle ] ]
        [ Html.text "page" ]


fromText : Html Msg
fromText =
    Html.p
        [ Attrs.css [ textStyle ] ]
        [ Html.text "from" ]


forText : Html Msg
forText =
    Html.p
        [ Attrs.css [ textStyle ] ]
        [ Html.text "for" ]


horizontalSeparator : Html Msg
horizontalSeparator =
    Grid.column
        [ flex (int 0) ]
        [ Html.div
            [ Attrs.css [ singleWidth ] ]
            []
        ]


textStyle : Style
textStyle =
    [ Style.hfnss
    , lineHeight (px 32)
    , singleWidth
    , textAlign center
    ]
        |> Css.batch


playForField : String -> Html Msg
playForField playForBeats =
    Html.input
        [ Attrs.css
            [ margin2 (px 1) (px 0)
            , Style.hfnss
            , color Colors.point0
            , Style.fontSmoothingNone
            , singleWidth
            ]
        , Attrs.value playForBeats
        , Attrs.spellcheck False
        , Events.onInput PlayForFieldUpdated
        ]
        []


playFromField : String -> Html Msg
playFromField playFromBeat =
    Html.input
        [ Attrs.css
            [ margin2 (px 1) (px 0)
            , Style.hfnss
            , color Colors.point0
            , Style.fontSmoothingNone
            , singleWidth
            ]
        , Attrs.value playFromBeat
        , Attrs.spellcheck False
        , Events.onInput PlayFromFieldUpdated
        ]
        []


playButton : Html Msg
playButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick PlayClicked
        ]
        [ Html.text "play" ]


openButton : Html Msg
openButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick OpenClicked
        ]
        [ Html.text "open" ]


buildButton : Html Msg
buildButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick BuildClicked
        ]
        [ Html.text "build" ]


saveButton : Html Msg
saveButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick SaveClicked
        ]
        [ Html.text "save" ]


trackersButton : Page -> Html Msg
trackersButton page =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , dent page Model.Trackers
            , doubleWidth
            ]
        , Events.onClick (PageClicked Model.Trackers)
        ]
        [ Html.text "trackers" ]


packageButton : Page -> Html Msg
packageButton page =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , dent page Model.Package
            , doubleWidth
            ]
        , Events.onClick (PageClicked Model.Package)
        ]
        [ Html.text "package" ]


newPartButton : Html Msg
newPartButton =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , doubleWidth
            ]
        , Events.onClick NewPartClicked
        ]
        [ Html.text "new part" ]


newTrackerButton : Html Msg
newTrackerButton =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , doubleWidth
            ]
        , Events.onClick NewTrackerClicked
        ]
        [ Html.text "new tracker" ]


doubleWidth : Style
doubleWidth =
    width (px (Style.noteWidth Style.Big * 2))


singleWidth : Style
singleWidth =
    width (px (Style.noteWidth Style.Big))


buttonStyle : Style
buttonStyle =
    [ Style.basicButton Style.Big
    , Style.hfnss
    , margin (px 1)
    , singleWidth
    , height (px (Style.noteHeight Style.Big + 4))
    , active [ Style.indent ]
    ]
        |> Css.batch


dent : Page -> Page -> Style
dent currentPage thisPage =
    if currentPage == thisPage then
        Style.indent

    else
        Style.outdent
