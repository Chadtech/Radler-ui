module Data.Error exposing
    ( Error(..)
    , toString
    )

-- TYPES --

import Json.Decode as Decode


type Error
    = ScoreDidNotSave
    | BackendHadProblemWithScore String
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

        BackendHadProblemWithScore str ->
            str

        MsgDecodeError decodeError ->
            [ "There was a problem decoding an incoming message from JS -> "
            , Decode.errorToString decodeError
            ]
                |> String.concat
