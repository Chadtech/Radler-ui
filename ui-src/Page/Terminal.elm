module Page.Terminal exposing
    ( Msg
    , onCmdEnterPressed
    , update
    , view
    )

import Api
import Css exposing (..)
import Data.Error as Error
import Data.Width as Width
import Html.Grid as Grid
import Model exposing (Model)
import Service.Api as Api
import Style
import Util.Cmd as CmdUtil
import View.Button as Button
import View.TextArea as TextArea



-- TYPES --


type Msg
    = TerminalUpdated String
    | ExecuteClicked
    | GotExecuteResponse (Result Api.Error ())



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TerminalUpdated str ->
            model
                |> Model.setTerminal str
                |> CmdUtil.withNoCmd

        ExecuteClicked ->
            attemptToExecute model

        GotExecuteResponse response ->
            model
                |> Model.setBackendStatusIdle
                |> handleResponse response
                |> CmdUtil.withNoCmd


handleResponse : Result Api.Error () -> Model -> Model
handleResponse response model =
    case response of
        Ok () ->
            model

        Err error ->
            model
                |> terminalExecutionFailed error


terminalExecutionFailed : Api.Error -> Model -> Model
terminalExecutionFailed error =
    error
        |> Error.ApiError
        |> Model.setError


onCmdEnterPressed : Model -> ( Model, Cmd Msg )
onCmdEnterPressed =
    attemptToExecute


attemptToExecute : Model -> ( Model, Cmd Msg )
attemptToExecute =
    Api.sendTerminal GotExecuteResponse



-- VIEW --


view : Model -> List (Grid.Column Msg)
view model =
    [ Grid.column
        [ flexDirection column
        , margin (px 2)
        ]
        [ Grid.row
            [ Style.fullWidth
            , flex (int 0)
            , flexBasis auto
            , marginBottom (px 5)
            ]
            [ Grid.column
                []
                [ Button.config ExecuteClicked "execute"
                    |> Button.withWidth Width.double
                    |> Button.toHtml
                ]
            ]
        , Grid.row
            [ Style.fullWidth
            , flex (int 1)
            ]
            [ Grid.column
                []
                [ TextArea.config TerminalUpdated model.terminal
                    |> TextArea.toHtml
                ]
            ]
        ]
    ]
