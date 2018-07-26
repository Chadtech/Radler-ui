module Data.Flags
    exposing
        ( Flags
        , decoder
        )

import Array exposing (Array)
import Data.Package as Package exposing (Package)
import Data.Part as Part exposing (Part)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP


-- TYPES --


type alias Flags =
    { package : Package
    , parts : Array Part
    }



-- DECODER --


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> JDP.required "package" Package.decoder
        |> JDP.required "parts" partsDecoder


partsDecoder : Decoder (Array Part)
partsDecoder =
    Part.decoder
        |> D.list
        |> D.map Array.fromList
        |> D.map atLeastOnePart


atLeastOnePart : Array Part -> Array Part
atLeastOnePart parts =
    if Array.isEmpty parts then
        [ Part.empty ]
            |> Array.fromList
    else
        parts
