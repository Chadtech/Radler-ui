module Data.Index exposing
    ( Index
    , decoder
    , encode
    , fromInt
    , next
    , previous
    , toEntries
    , toInt
    , toString
    , zero
    )

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- TYPES --


type Index a
    = Index Int



-- HELPERS --


toInt : Index a -> Int
toInt (Index index) =
    index


fromInt : Int -> Index a
fromInt =
    Index


toEntries : Array a -> List ( Index a, a )
toEntries =
    Array.toIndexedList >> List.map (Tuple.mapFirst fromInt)


toString : Index a -> String
toString =
    toInt >> String.fromInt


zero : Index a
zero =
    Index 0


next : Index a -> Index a
next (Index int) =
    Index <| int + 1


previous : Index a -> Index a
previous (Index int) =
    Index <| int - 1


encode : Index a -> Encode.Value
encode (Index int) =
    Encode.int int


decoder : Decoder (Index a)
decoder =
    Decode.map fromInt Decode.int
