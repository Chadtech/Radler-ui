module Data.Sheet
    exposing
        ( Sheet
        , addRow
        , columnCount
        , empty
        , mapRow
        , removeRow
        )

import Array exposing (Array)


-- TYPES --


type alias Sheet =
    { name : String
    , rows : Array (Array String)
    }


empty : Sheet
empty =
    { name = "doink"
    , rows =
        emptyRow 6
            |> Array.repeat 64
    }


emptyRow : Int -> Array String
emptyRow length =
    Array.repeat length ""



-- HELPERS --


columnCount : Sheet -> Int
columnCount { rows } =
    rows
        |> Array.get 0
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


removeRow : Int -> Sheet -> Sheet
removeRow index sheet =
    { sheet
        | rows =
            sheet.rows
                |> Array.slice
                    (index + 1)
                    (Array.length sheet.rows)
                |> Array.append
                    (pushEmptyRow (Array.slice 0 index sheet.rows))
    }


addRow : Int -> Sheet -> Sheet
addRow index sheet =
    { sheet
        | rows =
            sheet.rows
                |> Array.slice index (Array.length sheet.rows)
                |> Array.append
                    (pushEmptyRow (Array.slice 0 index sheet.rows))
    }


pushEmptyRow : Array (Array String) -> Array (Array String)
pushEmptyRow rows =
    case Array.get 0 rows of
        Just row ->
            Array.push
                (emptyRow (Array.length row))
                rows

        Nothing ->
            rows


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
