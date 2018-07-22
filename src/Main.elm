module Main exposing (..)

import Browser
import Browser.Navigation
import Data.Flags as Flags
import Json.Decode as D
import Model exposing (Model)
import Msg exposing (Msg(..))
import Ports exposing (JsMsg(..))
import Return2 as R2
import Update exposing (update)
import View exposing (view)


-- MAIN --


main : Program D.Value (Result D.Error Model) Msg
main =
    { init = init
    , subscriptions = subscriptions
    , update = updateOk
    , view = view
    }
        |> Browser.document


updateOk : Msg -> Result D.Error Model -> ( Result D.Error Model, Cmd Msg )
updateOk msg result =
    case result of
        Ok model ->
            update msg model
                |> R2.mapModel Ok

        Err err ->
            Err err
                |> R2.withNoCmd


init : D.Value -> ( Result D.Error Model, Cmd Msg )
init json =
    json
        |> D.decodeValue Flags.decoder
        |> Result.map Model.init
        |> R2.withNoCmd



-- SUBSCRIPTIONS --


subscriptions : Result D.Error Model -> Sub Msg
subscriptions result =
    case result of
        Ok model ->
            Ports.fromJs Msg.decode

        Err _ ->
            Sub.none
