module Ui.Header exposing
    ( Msg
    , update
    , view
    )

import Api
import Array
import BackendStatus
import Colors
import Css exposing (..)
import Data.Error as Error
import Data.Modal as Modal
import Data.Package as Package
import Data.Page as Page exposing (Page)
import Data.Tracker as Tracker
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style
import Util



-- TYPES --


type Msg
    = PageClicked Page
    | NewPartClicked
    | NewTrackerClicked
    | PlayClicked
    | SaveClicked
    | BuildClicked
    | PlayFromFieldUpdated String
    | PlayForFieldUpdated String
    | PlaySent (Result Api.Error ())



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageClicked page ->
            { model | page = page }
                |> Util.withNoCmd

        NewPartClicked ->
            model
                |> Model.addNewPart
                |> Util.withNoCmd

        NewTrackerClicked ->
            { model
                | trackers =
                    Array.push
                        (Tracker.init Style.Small 0)
                        model.trackers
            }
                |> Util.withNoCmd

        PlayClicked ->
            case Model.score model of
                Ok scoreStr ->
                    scoreStr
                        |> Play
                        |> sendHttp model
                        |> Util.withModel
                            (Model.setBackendStatusWorking model)

                Err newModel ->
                    newModel
                        |> Util.withNoCmd

        SaveClicked ->
            [ Package.saveToDisk
                model.package
            , Model.saveParts
                model
            ]
                |> Cmd.batch
                |> Util.withModel model

        BuildClicked ->
            model
                |> Model.setModal Modal.initBuild
                |> Util.withNoCmd

        PlayFromFieldUpdated str ->
            model
                |> Model.setPlayFrom str
                |> Util.withNoCmd

        PlayForFieldUpdated str ->
            model
                |> Model.setPlayFor str
                |> Util.withNoCmd

        PlaySent (Ok ()) ->
            model
                |> Model.setBackendStatusIdle
                |> Util.withNoCmd

        PlaySent (Err err) ->
            model
                |> playFailed err
                |> Model.setBackendStatusIdle
                |> Util.withNoCmd


playFailed : Api.Error -> Model -> Model
playFailed error =
    error
        |> Api.errorToString
        |> Error.BackendHadProblemWithScore
        |> Model.setError



-- HTTP --


type Call
    = Play String


sendHttp : Model -> Call -> Cmd Msg
sendHttp model call =
    case call of
        Play score ->
            Api.sendScore
                { endpoint = model.endpoints.play
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
            [ pageButton Page.Trackers model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ pageButton Page.Package model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ pageButton Page.Parts model.page ]
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


pageButton : Page -> Page -> Html Msg
pageButton thisPage currentPage =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , dent currentPage thisPage
            , doubleWidth
            ]
        , Events.onClick (PageClicked thisPage)
        ]
        [ Html.text <| Page.toString thisPage ]


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
