module Data.Flags exposing
    ( Flags
    , decoder
    )

import Array exposing (Array)
import Data.Package as Package exposing (Package)
import Data.Part as Part exposing (Part)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP



-- TYPES --


{-|

    Flags :=
        Flags are the values that the
        application is initialized from.

        They are decoded from json given
        to the Elm application. If that
        decoding fails the application is
        in big trouble.

-}
type alias Flags =
    { package : Package
    , parts : Array Part
    , enginePort : Int
    }



-- DECODER --


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> JDP.required "package" Package.decoder
        |> JDP.required "parts" partsDecoder
        |> JDP.required "enginePortNumber" D.int


partsDecoder : Decoder (Array Part)
partsDecoder =
    Part.decoder
        |> D.list
        |> D.map Array.fromList
        |> D.map atLeastOnePart


atLeastOnePart : Array Part -> Array Part
atLeastOnePart parts =
    if Array.isEmpty parts then
        [ Part.empty "new-part" ]
            |> Array.fromList

    else
        parts
