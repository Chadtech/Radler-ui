module Data.Error exposing
    ( Error(..)
    , toString
    )

import Api
import Json.Decode as Decode



-- TYPES --


type Error
    = ScoreDidNotSave
    | ApiError Api.Error
    | BackendIsBusy
    | MsgDecodeError Decode.Error



-- HELPERS --


toString : Error -> String
toString error =
    case error of
        ScoreDidNotSave ->
            """
            The score failed to save.
            This is probably because the score includes
            a part that is not loaded into Radler
            """

        ApiError endpointError ->
            Api.errorToString endpointError

        BackendIsBusy ->
            "I am currently busy doing something else. Please wait for me to finish."

        MsgDecodeError decodeError ->
            [ "There was a problem decoding an incoming message from JS -> "
            , Decode.errorToString decodeError
            ]
                |> String.concat
