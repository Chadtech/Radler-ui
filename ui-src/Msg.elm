module Msg exposing
    ( Msg(..)
    , decode
    , escapePressedDecoder
    )

import Data.Error as Error exposing (Error)
import Data.Index exposing (Index)
import Data.Tracker exposing (Tracker)
import Json.Decode as Decode exposing (Decoder)
import Page.Package as Package
import Page.Parts as Parts
import Ui.Header as Header
import Ui.Modal as Modal
import Ui.Tracker as Tracker



-- TYPES --


type Msg
    = MsgDecodeFailed Error
    | TrackerMsg (Index Tracker) Tracker.Msg
    | HeaderMsg Header.Msg
    | PackageMsg Package.Msg
    | PartsMsg Parts.Msg
    | ModalMsg Modal.Msg
    | EscapePressed



-- DECODERS --


escapePressedDecoder : Decoder Msg
escapePressedDecoder =
    let
        fromString : String -> Decoder Msg
        fromString str =
            case str of
                "Escape" ->
                    Decode.succeed EscapePressed

                _ ->
                    Decode.fail "Key is not escape"
    in
    Decode.string
        |> Decode.field "key"
        |> Decode.andThen fromString


decode : Decode.Value -> Msg
decode json =
    case Decode.decodeValue decoder json of
        Ok msg ->
            msg

        Err err ->
            MsgDecodeFailed <| Error.MsgDecodeError err


decoder : Decoder Msg
decoder =
    let
        payloadDecoder : String -> Decoder Msg
        payloadDecoder type_ =
            [ Modal.msgDecoderFromType type_
                |> Decode.map ModalMsg
            ]
                |> Decode.oneOf
                |> Decode.field "payload"
    in
    Decode.string
        |> Decode.field "type"
        |> Decode.andThen payloadDecoder
