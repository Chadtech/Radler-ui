module Data.Sheet
    exposing
        ( Sheet
        , columnCount
        , empty
        )

import Array exposing (Array)


type alias Sheet =
    { name : String
    , rows : Array (Array String)
    }


empty : Sheet
empty =
    { name = "doink"
    , rows =
        Array.repeat 4 ""
            |> Array.repeat 64
    }


columnCount : Sheet -> Int
columnCount { rows } =
    rows
        |> Array.get 0
        |> Maybe.map Array.length
        |> Maybe.withDefault 0
