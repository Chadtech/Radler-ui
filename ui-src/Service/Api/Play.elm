module Service.Api.Play exposing
    ( Msg
    , attempt
    , update
    )

import Api
import Data.Error as Error
import Model exposing (Model)
import Service.Api
import Util.Cmd as CmdUtil



-- TYPES --


type Msg
    = GotPlayResponse (Result Api.Error ())



-- PUBLIC HELPERS --


attempt : Model -> ( Model, Cmd Msg )
attempt model =
    case Model.score model of
        Ok scoreStr ->
            Service.Api.sendPlay
                { score = scoreStr
                , handler = GotPlayResponse
                }
                model

        Err newModel ->
            newModel
                |> CmdUtil.withNoCmd



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlayResponse response ->
            let
                idleModel : Model
                idleModel =
                    Model.setBackendStatusIdle model
            in
            case response of
                Ok () ->
                    if model.repeatPlayback then
                        attempt idleModel

                    else
                        idleModel
                            |> CmdUtil.withNoCmd

                Err error ->
                    idleModel
                        |> Model.setError (Error.ApiError error)
                        |> CmdUtil.withNoCmd
