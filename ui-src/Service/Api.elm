module Service.Api exposing
    ( ScorePayload
    , sendBuild
    , sendPlay
    , sendScore
    , sendTerminal
    )

import Api
import BackendStatus
import Data.Error as Error
import Model exposing (Model)
import Util.Cmd as CmdUtil


sendPlay : ScorePayload msg -> Model -> ( Model, Cmd msg )
sendPlay payload =
    send (Play payload)


sendBuild : ScorePayload msg -> Model -> ( Model, Cmd msg )
sendBuild payload =
    send (Build payload)


sendTerminal : (Result Api.Error () -> msg) -> Model -> ( Model, Cmd msg )
sendTerminal handler =
    send (Terminal handler)



-- TYPES --


type Call msg
    = Play (ScorePayload msg)
    | Build (ScorePayload msg)
    | Terminal (Result Api.Error () -> msg)


type alias ScorePayload msg =
    { score : String
    , handler : Result Api.Error () -> msg
    }



-- PRIVATE HELPERS --


send : Call msg -> Model -> ( Model, Cmd msg )
send call model =
    if model.backendStatus == BackendStatus.Idle then
        ( Model.setBackendStatusWorking model
        , callToCmd call model
        )

    else
        Model.setError Error.BackendIsBusy model
            |> CmdUtil.withNoCmd


callToCmd : Call msg -> Model -> Cmd msg
callToCmd call model =
    case call of
        Play payload ->
            sendScore model.endpoints.play payload

        Build payload ->
            sendScore model.endpoints.build payload

        Terminal handler ->
            Api.post
                { endpoint = model.endpoints.terminal
                , body = model.terminal
                , expect = handler
                }


sendScore : Api.Endpoint -> ScorePayload msg -> Cmd msg
sendScore endpoint { score, handler } =
    Api.post
        { endpoint = endpoint
        , body = score
        , expect = handler
        }
