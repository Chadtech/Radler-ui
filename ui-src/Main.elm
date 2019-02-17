module Main exposing (main)

import Browser
import Browser.Navigation
import Data.Flags as Flags
import Html.Styled as Html
import Json.Decode as D
import Model exposing (Model)
import Msg exposing (Msg)
import Ports exposing (JsMsg)
import Return2 as R2
import Style
import Ui.Error exposing (initializationErrorView)
import Update exposing (update)
import View exposing (view)



-- MAIN --


main : Program D.Value (Result D.Error Model) Msg
main =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }
        |> Browser.document


init : D.Value -> ( Result D.Error Model, Cmd Msg )
init json =
    json
        |> D.decodeValue Flags.decoder
        |> Result.map Model.init
        |> R2.withNoCmd


subscriptions : Result D.Error Model -> Sub Msg
subscriptions result =
    case result of
        Ok model ->
            Ports.fromJs Msg.decode

        Err _ ->
            Sub.none


update : Msg -> Result D.Error Model -> ( Result D.Error Model, Cmd Msg )
update msg result =
    case result of
        Ok model ->
            Update.update msg model
                |> R2.mapModel Ok

        Err err ->
            Err err
                |> R2.withNoCmd


view : Result D.Error Model -> Browser.Document Msg
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
