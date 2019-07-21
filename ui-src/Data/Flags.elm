module Data.Flags exposing
    ( Flags
    , decoder
    )

import Api
import Array exposing (Array)
import Data.Index as Index
import Data.Package as Package exposing (Package)
import Data.Part as Part exposing (Part)
import Data.Size as Size
import Data.Tracker as Tracker exposing (Tracker)
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
    , trackers : Array Tracker
    , endpoints : Api.Endpoints
    , terminal : String
    }



-- DECODER --


decoder : Decoder Flags
decoder =
    Decode.map5 Flags
        (Decode.field "package" Package.decoder)
        (Decode.field "parts" partsDecoder)
        (Decode.field "trackers" trackersDecoder)
        (Decode.field "enginePortNumber" endpointsDecoder)
        (Decode.field "terminal" Decode.string)


endpointsDecoder : Decoder Api.Endpoints
endpointsDecoder =
    Decode.map Api.fromPortNumber Decode.int


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


trackersDecoder : Decoder (Array Tracker)
trackersDecoder =
    let
        atLeastTwoTrackers : Array Tracker -> Array Tracker
        atLeastTwoTrackers trackers =
            if Array.isEmpty trackers then
                [ Tracker.init Size.big Index.zero
                , Tracker.init Size.big Index.zero
                ]
                    |> Array.fromList

            else
                trackers
    in
    Tracker.decoder
        |> Decode.list
        |> Decode.map
            (Array.fromList >> atLeastTwoTrackers)
