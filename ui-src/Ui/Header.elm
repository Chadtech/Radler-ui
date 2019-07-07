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
            model
                |> Model.addNewTracker
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
            Model.save model
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
        , flexDirection Css.column
        ]
        [ playbackButtons model
        , uiButtons model
        ]


playbackButtons : Model -> Html Msg
playbackButtons model =
    Grid.row
        []
        [ column <| button SaveClicked "save"
        , column <| button PlayClicked "play"
        , column <| text "from"
        , column <|
            input
                PlayForFieldUpdated
                model.playFromBeatField
        , column <| text "for"
        , column <|
            input
                PlayForFieldUpdated
                model.playForBeatsField
        , column <| button BuildClicked "build"
        , column <| repeatCheckbox
        , Grid.column
            [ alignItems flexEnd
            , flexDirection Css.column
            ]
            [ BackendStatus.view model.backendStatus ]
        ]


uiButtons : Model -> Html Msg
uiButtons model =
    Grid.row
        []
        [ column <| text "play"
        , column <| pageButton Route.Trackers model.page
        , column <| pageButton Route.Package model.page
        , column <| pageButton Route.Parts model.page
        , horizontalSeparator
        , column <| newTrackerButton
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


button : Msg -> String -> Html Msg
button msg label =
    Button.button msg label
        |> Button.withWidth Button.singleWidth
        |> Button.makeTallerBy 4
        |> Button.toHtml


pageButton : Route -> Page -> Html Msg
pageButton route currentPage =
    Button.button (RouteClicked route) (Route.toString route)
        |> Button.withWidth Button.doubleWidth
        |> Button.makeTallerBy 4
        |> Button.indent (Route.isPage currentPage route)
        |> Button.toHtml


newTrackerButton : Html Msg
newTrackerButton =
    Button.button NewTrackerClicked "new tracker"
        |> Button.withWidth Button.doubleWidth
        |> Button.makeTallerBy 4
        |> Button.toHtml


repeatCheckbox : Html Msg
repeatCheckbox =
    Html.text ""
