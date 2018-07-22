module Data.Flags
    exposing
        ( Flags
        , decoder
        )

import Data.Package as Package exposing (Package)
import Json.Decode as D exposing (Decoder)


-- TYPES --


type alias Flags =
    { package : Package }



-- DECODER --


decoder : Decoder Flags
decoder =
    D.string
        |> D.andThen Package.decoder
        |> D.field "package"
        |> D.map Flags
