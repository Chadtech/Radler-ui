module Data.Flags exposing
    ( Flags
    , decoder
    )

import Api
import Array exposing (Array)
import Data.Package as Package exposing (Package)
import Data.Part as Part exposing (Part)
import Json.Decode as Decode exposing (Decoder)



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
    , endpoints : Api.Endpoints
    }



-- DECODER --


decoder : Decoder Flags
decoder =
    Decode.map3 Flags
        (Decode.field "package" Package.decoder)
        (Decode.field "parts" partsDecoder)
        (Decode.field "enginePortNumber" Api.endpointsFromPortNumberDecoder)


partsDecoder : Decoder (Array Part)
partsDecoder =
    let
        atLeastOnePart : Array Part -> Array Part
        atLeastOnePart parts =
            if Array.isEmpty parts then
                [ Part.empty "new-part" ]
                    |> Array.fromList

            else
                parts
    in
    Part.decoder
        |> Decode.list
        |> Decode.map
            (Array.fromList >> atLeastOnePart)
