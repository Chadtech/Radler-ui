module Data.Beat
    exposing
        ( Beat
        , empty
        , fromString
        , setNote
        , toString
        )

import Array exposing (Array)


-- TYPES --


type alias Beat =
    Array String



-- HELPERS --


toString : Array String -> String
toString =
    Array.toList >> String.join ","


fromString : String -> Array String
fromString =
    String.split "," >> Array.fromList


empty : Int -> Array String
empty length =
    Array.repeat length ""


setNote : Int -> String -> Beat -> Beat
setNote =
    Array.set
