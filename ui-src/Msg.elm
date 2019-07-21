module Msg exposing
    ( Msg(..)
    , decode
    , keyDecoder
    )

import Data.Error as Error exposing (Error)
import Data.Index exposing (Index)
import Data.Tracker exposing (Tracker)
import Json.Decode as Decode exposing (Decoder)
import Page.Package as Package
import Page.Parts as Parts
import Page.Terminal as Terminal
import Service.Api.Play as Play
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
    | TerminalMsg Terminal.Msg
    | ModalMsg Modal.Msg
    | EscapePressed
    | CmdEnterPressed
    | PlayMsg Play.Msg



-- DECODERS --


keyDecoder : Decoder Msg
keyDecoder =
    [ escapePressedDecoder
    , cmdEnterPressedDecoder
    ]
        |> Decode.oneOf


escapePressedDecoder : Decoder Msg
escapePressedDecoder =
    let
        fromString : String -> Decoder Msg
        fromString str =
            if str == "Escape" then
                Decode.succeed EscapePressed

            else
                Decode.fail "Key is not escape"
    in
    Decode.string
        |> Decode.field "key"
        |> Decode.andThen fromString


cmdEnterPressedDecoder : Decoder Msg
cmdEnterPressedDecoder =
    let
        fromStringAndMeta : ( Bool, String ) -> Decoder Msg
        fromStringAndMeta tuple =
            case tuple of
                ( True, "Enter" ) ->
                    Decode.succeed CmdEnterPressed

                _ ->
                    Decode.fail "Key is not enter, or meta not pressed"
    in
    Decode.map2 Tuple.pair
        (Decode.field "meta" Decode.bool)
        (Decode.field "key" Decode.string)
        |> Decode.andThen fromStringAndMeta


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
