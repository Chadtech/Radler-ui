module Ui.Header exposing
    ( Msg
    , update
    , view
    )

import Api
import BackendStatus
import Css exposing (..)
import Data.Error as Error
import Data.Modal as Modal
import Data.Page as Page exposing (Page)
import Data.Route as Route exposing (Route)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style
import Util.Cmd as CmdUtil
import View.Button as Button
import View.Checkbox as Checkbox



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
    | RepeatClicked



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteClicked route ->
            model
                |> Model.setPage (Route.toPage route)
                |> CmdUtil.withNoCmd

        NewTrackerClicked ->
            model
                |> Model.addNewTracker
                |> CmdUtil.withNoCmd

        PlayClicked ->
            attemptToPlay model

        SaveClicked ->
            model
                |> Model.save
                |> CmdUtil.withModel model

        BuildClicked ->
            model
                |> Model.setModal Modal.initBuild
                |> CmdUtil.withNoCmd

        PlayFromFieldUpdated "" ->
            model
                |> Model.setPlayFrom 0
                |> CmdUtil.withNoCmd

        PlayFromFieldUpdated str ->
            case String.toInt str of
                Just playFrom ->
                    model
                        |> Model.setPlayFrom playFrom
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        PlayForFieldUpdated "" ->
            model
                |> Model.setPlayFor 0
                |> CmdUtil.withNoCmd

        PlayForFieldUpdated str ->
            case String.toInt str of
                Just playFor ->
                    model
                        |> Model.setPlayFor playFor
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        PlaySent (Ok ()) ->
            if model.repeatPlayback then
                attemptToPlay model

            else
                model
                    |> Model.setBackendStatusIdle
                    |> CmdUtil.withNoCmd

        PlaySent (Err err) ->
            model
                |> playFailed err
                |> Model.setBackendStatusIdle
                |> CmdUtil.withNoCmd

        RepeatClicked ->
            model
                |> Model.toggleRepeatPlayback
                |> CmdUtil.withNoCmd


attemptToPlay : Model -> ( Model, Cmd Msg )
attemptToPlay model =
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
        , flexDirection Css.column
        ]
        [ Grid.row [] (playbackButtons model)
        , Grid.row [] (uiButtons model)
        ]


playbackButtons : Model -> List (Html Msg)
playbackButtons model =
    [ buttonColumn SaveClicked "save"
    , buttonColumn PlayClicked "play"
    , textColumn "from"
    , inputColumn PlayFromFieldUpdated <|
        String.fromInt model.playFromBeat
    , textColumn "for"
    , inputColumn PlayForFieldUpdated <|
        String.fromInt model.playForBeats
    , Grid.column
        [ flex none
        , padding (px 1)
        , Style.doubleWidth Style.Big
        ]
        [ text "repeat"
        , Checkbox.checkbox RepeatClicked model.repeatPlayback
            |> Checkbox.toHtml
            |> column
        ]
    , buttonColumn BuildClicked "build"
    , Grid.column
        [ alignItems flexEnd
        , flexDirection Css.column
        ]
        [ BackendStatus.view model.backendStatus ]
    ]


uiButtons : Model -> List (Html Msg)
uiButtons model =
    [ textColumn "page"
    , pageButtonColumn Route.Trackers model.page
    , pageButtonColumn Route.Package model.page
    , pageButtonColumn Route.Parts model.page
    , horizontalSeparator
    , Button.button NewTrackerClicked "new tracker"
        |> Button.withWidth Button.doubleWidth
        |> Button.makeTallerBy 4
        |> Button.toHtml
        |> column
    ]


column : Html Msg -> Html Msg
column childButton =
    Grid.column
        [ flex (int 0)
        , padding (px 1)
        ]
        [ childButton ]


text : String -> Html Msg
text str =
    Html.p
        [ Attrs.css
            [ Style.hfnss
            , lineHeight (px 32)
            , Style.singleWidth Style.Big
            , textAlign center
            ]
        ]
        [ Html.text str ]


textColumn : String -> Html Msg
textColumn =
    column << text


horizontalSeparator : Html Msg
horizontalSeparator =
    Grid.column
        [ flex (int 0) ]
        [ Html.div
            [ Attrs.css
                [ Style.singleWidth Style.Big ]
            ]
            []
        ]


input : (String -> Msg) -> String -> Html Msg
input msgCtor value =
    Html.input
        [ Attrs.css
            [ Style.hfnss
            , Style.singleWidth Style.Big
            , height (px 30)
            ]
        , Attrs.value value
        , Attrs.spellcheck False
        , Events.onInput msgCtor
        ]
        []


inputColumn : (String -> Msg) -> String -> Html Msg
inputColumn msgCtor value =
    column <| input msgCtor value


button : Msg -> String -> Html Msg
button msg label =
    Button.button msg label
        |> Button.withWidth Button.singleWidth
        |> Button.makeTallerBy 4
        |> Button.toHtml


buttonColumn : Msg -> String -> Html Msg
buttonColumn msg label =
    column <| button msg label


pageButton : Route -> Page -> Html Msg
pageButton route currentPage =
    Button.button (RouteClicked route) (Route.toString route)
        |> Button.withWidth Button.doubleWidth
        |> Button.makeTallerBy 4
        |> Button.indent (Route.isPage currentPage route)
        |> Button.toHtml


pageButtonColumn : Route -> Page -> Html Msg
pageButtonColumn route currentPage =
    column <| pageButton route currentPage
