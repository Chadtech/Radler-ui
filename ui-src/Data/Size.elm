module Data.Size exposing
    ( Size(..)
    , big
    , decoder
    , encode
    , small
    , toUnitHeight
    , toUnitWidth
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


toUnitWidth : Size -> Float
toUnitWidth size =
    case size of
        Big ->
            90

        Small ->
            60


toUnitHeight : Size -> Float
toUnitHeight size =
    case size of
        Big ->
            26

        Small ->
            16


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
