module Data.Beat exposing
    ( Beat
    , addNote
    , empty
    , fromList
    , fromString
    , length
    , removeNote
    , setNote
    , toIndexedList
    , toList
    , toString
    )

import Array exposing (Array)
import Data.Note as Note exposing (Note)



-- TYPES --


{-|

    Beat :=
        One index of time in a piece of music

        In this software a beat is treated as
        a row, and each row contains what note
        each voice in the music is meant to play
        (or not play, if there is no note)

-}
type Beat
    = Beat (Array Note)



-- HELPERS --


toList : Beat -> List Note
toList (Beat beat) =
    Array.toList beat


toIndexedList : Beat -> List ( Int, Note )
toIndexedList (Beat beat) =
    Array.toIndexedList beat


fromList : List Note -> Beat
fromList =
    Array.fromList >> Beat


length : Beat -> Int
length (Beat beat) =
    Array.length beat


addNote : Int -> Beat -> Beat
addNote index (Beat beat) =
    Array.append
        (Array.push Note.empty (Array.slice 0 (index + 1) beat))
        (Array.slice (index + 1) (Array.length beat) beat)
        |> Beat


removeNote : Int -> Beat -> Beat
removeNote index (Beat beat) =
    beat
        |> Array.slice (index + 1) (Array.length beat)
        |> Array.append (Array.slice 0 index beat)
        |> Beat


toString : Beat -> String
toString (Beat beat) =
    beat
        |> Array.toList
        |> List.map Note.toString
        |> String.join noteDelimiter


fromString : String -> Beat
fromString str =
    str
        |> String.split noteDelimiter
        |> List.map Note.fromString
        |> Array.fromList
        |> Beat


noteDelimiter : String
noteDelimiter =
    ";"


empty : Int -> Beat
empty thisLength =
    Array.repeat thisLength Note.empty
        |> Beat


setNote : Int -> Note -> Beat -> Beat
setNote index note (Beat beat) =
    Array.set index note beat
        |> Beat
