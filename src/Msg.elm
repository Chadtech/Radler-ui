module Msg
    exposing
        ( Msg(..)
        , decode
        )

import Html.Header as Header
import Html.Package as Package
import Html.Tracker as Tracker
import Json.Decode as D exposing (Decoder)


type Msg
    = MsgDecodeFailed D.Error
    | TrackerMsg Int Tracker.Msg
    | HeaderMsg Header.Msg
    | PackageMsg Package.Msg


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
