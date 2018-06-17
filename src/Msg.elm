module Msg
    exposing
        ( Msg(..)
        , decode
        )

import Json.Decode as D exposing (Decoder)


type Msg
    = MsgDecodeFailed D.Error


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
