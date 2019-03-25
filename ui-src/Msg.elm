module Msg exposing
    ( Msg(..)
    , decode
    )

import Json.Decode as Decode exposing (Decoder)
import Page.Package as Package
import Page.Parts as Parts
import Ui.Header as Header
import Ui.Modal as Modal
import Ui.Tracker as Tracker


type Msg
    = MsgDecodeFailed Decode.Error
    | TrackerMsg Int Tracker.Msg
    | HeaderMsg Header.Msg
    | PackageMsg Package.Msg
    | PartsMsg Parts.Msg
    | ModalMsg Modal.Msg


decode : Decode.Value -> Msg
decode json =
    case Decode.decodeValue decoder json of
        Ok msg ->
            msg

        Err err ->
            MsgDecodeFailed err


decoder : Decoder Msg
decoder =
    Decode.string
        |> Decode.field "type"
        |> Decode.andThen
            (Decode.field "payload" << toMsg)


toMsg : String -> Decoder Msg
toMsg type_ =
    case type_ of
        "partDeleted" ->
            [ Decode.map Ok Decode.int
            , Decode.map Err Decode.string
            ]
                |> Decode.oneOf
                |> Decode.map
                    (ModalMsg << Modal.partDeleted)

        _ ->
            ("Unrecognized Msg type -> " ++ type_)
                |> Decode.fail
