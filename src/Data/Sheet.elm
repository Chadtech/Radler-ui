module Data.Sheet
    exposing
        ( Sheet
        , addColumn
        , addRow
        , columnCount
        , empty
        , mapRow
        , removeColumn
        , removeRow
        , setName
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


addColumn : Int -> Sheet -> Sheet
addColumn index sheet =
    { sheet
        | rows =
            Array.map (addColumnToRow index) sheet.rows
    }


addColumnToRow : Int -> Array String -> Array String
addColumnToRow index row =
    row
        |> Array.slice (index + 1) (Array.length row)
        |> Array.append
            (Array.push "" (Array.slice 0 (index + 1) row))


removeColumn : Int -> Sheet -> Sheet
removeColumn index sheet =
    { sheet
        | rows =
            Array.map (removeColumnFromRow index) sheet.rows
    }


removeColumnFromRow : Int -> Array String -> Array String
removeColumnFromRow index row =
    row
        |> Array.slice (index + 1) (Array.length row)
        |> Array.append (Array.slice 0 index row)


setName : String -> Sheet -> Sheet
setName str sheet =
    { sheet | name = str }


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
                    (Array.slice 0 index sheet.rows)
    }


addRow : Int -> Sheet -> Sheet
addRow index sheet =
    case Maybe.map Array.length <| Array.get 0 sheet.rows of
        Just rowLength ->
            let
                ni =
                    index + 1
            in
            { sheet
                | rows =
                    sheet.rows
                        |> Array.slice ni (Array.length sheet.rows)
                        |> Array.append
                            (pushEmptyRow rowLength (Array.slice 0 ni sheet.rows))
            }

        Nothing ->
            sheet


pushEmptyRow : Int -> Array (Array String) -> Array (Array String)
pushEmptyRow columnNumber rows =
    Array.push
        (emptyRow columnNumber)
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
