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
import Data.Route as Route exposing (Route)
import Data.Tracker as Tracker
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Page.Parts.Model as Parts
import Style
import Util.Cmd as CmdUtil



-- TYPES --


type Msg
    = RouteClicked Route
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
        RouteClicked route ->
            model
                |> Model.setPage (Route.toPage route)
                |> CmdUtil.withNoCmd

        NewTrackerClicked ->
            { model
                | trackers =
                    Array.push
                        (Tracker.init Style.Small 0)
                        model.trackers
            }
                |> CmdUtil.withNoCmd

        PlayClicked ->
            case Model.score model of
                Ok scoreStr ->
                    scoreStr
                        |> Play
                        |> sendHttp model
                        |> CmdUtil.withModel
                            (Model.setBackendStatusWorking model)

                Err newModel ->
                    newModel
                        |> CmdUtil.withNoCmd

        SaveClicked ->
            [ Package.saveToDisk
                model.package
            , Model.saveParts
                model
            ]
                |> Cmd.batch
                |> CmdUtil.withModel model

        BuildClicked ->
            model
                |> Model.setModal Modal.initBuild
                |> CmdUtil.withNoCmd

        PlayFromFieldUpdated str ->
            model
                |> Model.setPlayFrom str
                |> CmdUtil.withNoCmd

        PlayForFieldUpdated str ->
            model
                |> Model.setPlayFor str
                |> CmdUtil.withNoCmd

        PlaySent (Ok ()) ->
            model
                |> Model.setBackendStatusIdle
                |> CmdUtil.withNoCmd

        PlaySent (Err err) ->
            model
                |> playFailed err
                |> Model.setBackendStatusIdle
                |> CmdUtil.withNoCmd


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
            [ pageButton Route.Trackers model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ pageButton Route.Package model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ pageButton Route.Parts model.page ]
        , horizontalSeparator
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


pageButton : Route -> Page -> Html Msg
pageButton route currentPage =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , dent currentPage route
            , doubleWidth
            ]
        , Events.onClick (RouteClicked route)
        ]
        [ Html.text <| Route.toString route ]


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
    width <| px <| Style.noteWidth Style.Big * 2


singleWidth : Style
singleWidth =
    width <| px <| Style.noteWidth Style.Big


buttonStyle : Style
buttonStyle =
    [ Style.clickableButtonStyle Style.Big
    , margin (px 1)
    , singleWidth
    , height (px (Style.noteHeight Style.Big + 4))
    ]
        |> Css.batch


dent : Page -> Route -> Style
dent currentPage route =
    if Route.isPage currentPage route then
        Style.indent

    else
        Style.outdent
