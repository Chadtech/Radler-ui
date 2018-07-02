module Msg
    exposing
        ( Msg(..)
        , decode
        )

import Header
import Json.Decode as D exposing (Decoder)
import Tracker


type Msg
    = MsgDecodeFailed D.Error
    | TrackerMsg Int Tracker.Msg
    | HeaderMsg Header.Msg


decode : D.Value -> Msg
decode json =
    case D.decodeValue decoder json of
        Ok msg ->
            msg

        Err err ->
            MsgDecodeFailed err


decoder : Decoder Msg
decoder =
    D.field "type" D.string
        |> D.andThen (D.field "payload" << toMsg)


toMsg : String -> Decoder Msg
toMsg type_ =
    case type_ of
        _ ->
            ("Unrecognized Msg type -> " ++ type_)
                |> D.fail
