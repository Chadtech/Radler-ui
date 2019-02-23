module Main exposing (main)

import Browser
import Data.Flags as Flags
import Html.Styled as Html
import Json.Decode as Decode
import Model exposing (Model)
import Msg exposing (Msg)
import Ports
import Style
import Ui.Error exposing (initializationErrorView)
import Update
import Util
import View



-- MAIN --


main : Program Decode.Value (Result Decode.Error Model) Msg
main =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }
        |> Browser.document


init : Decode.Value -> ( Result Decode.Error Model, Cmd Msg )
init json =
    json
        |> Decode.decodeValue Flags.decoder
        |> Result.map Model.init
        |> Util.withNoCmd


subscriptions : Result Decode.Error Model -> Sub Msg
subscriptions result =
    case result of
        Ok _ ->
            Ports.fromJs Msg.decode

        Err _ ->
            Sub.none


update : Msg -> Result Decode.Error Model -> ( Result Decode.Error Model, Cmd Msg )
update msg result =
    case result of
        Ok model ->
            Update.update msg model
                |> Util.mapModel Ok

        Err err ->
            Err err
                |> Util.withNoCmd


view : Result Decode.Error Model -> Browser.Document Msg
view result =
    case result of
        Ok model ->
            { title = "Radler"
            , body =
                View.view model
                    |> (::) Style.globals
                    |> List.map Html.toUnstyled
            }

        Err err ->
            { title = "Error"
            , body =
                [ Style.globals
                , initializationErrorView err
                ]
                    |> List.map Html.toUnstyled
            }
