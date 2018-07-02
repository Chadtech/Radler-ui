module Data.Sheet
    exposing
        ( Sheet
        , columnCount
        , empty
        , mapRow
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
        Array.repeat 6 ""
            |> Array.repeat 64
    }



-- HELPERS --


columnCount : Sheet -> Int
columnCount { rows } =
    rows
        |> Array.get 0
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


mapRow : Int -> (Array String -> Array String) -> Sheet -> Sheet
mapRow index f sheet =
    case Array.get index sheet.rows of
        Just row ->
            { sheet
                | rows =
                    Array.set
                        index
                        (f row)
                        sheet.rows
            }

        Nothing ->
            sheet
