module Data.Size exposing
    ( Size(..)
    , big
    , decoder
    , encode
    , small
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- TYPES --


type Size
    = Big
    | Small



-- VALUES --


big : Size
big =
    Big


small : Size
small =
    Small



-- HELPERS --


encode : Size -> Encode.Value
encode size =
    Encode.string <|
        case size of
            Big ->
                "big"

            Small ->
                "small"


decoder : Decoder Size
decoder =
    let
        fromString : String -> Decoder Size
        fromString str =
            case str of
                "big" ->
                    Decode.succeed Big

                "small" ->
                    Decode.succeed Small

                _ ->
                    Decode.fail "not big or small"
    in
    Decode.string
        |> Decode.andThen fromString
