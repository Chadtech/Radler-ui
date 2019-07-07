module Data.Beat exposing
    ( Beat
    , addNoteAfter
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
import Data.Encoding as Encoding
import Data.Index as Index exposing (Index)
import Data.Note as Note exposing (Note)
import Util.Array as ArrayUtil



-- TYPES --


{-|

    Beat :=
        One index of time in a piece of music

        In this software a beat is treated as
        a row, and each row contains what note
        each voice in the music is meant to play
        (or not play, if there is no note)

-}
type Beat encoding
    = Beat (Array (Note encoding))



-- HELPERS --


toList : Beat encoding -> List (Note encoding)
toList (Beat beat) =
    Array.toList beat


toIndexedList : Beat encoding -> List ( Index (Note encoding), Note encoding )
toIndexedList (Beat beat) =
    Index.toEntries beat


fromList : List (Note encoding) -> Beat encoding
fromList =
    Array.fromList >> Beat


length : Beat encoding -> Int
length (Beat beat) =
    Array.length beat


addNoteAfter : Index (Note Encoding.None) -> Beat Encoding.None -> Beat Encoding.None
addNoteAfter index (Beat beat) =
    ArrayUtil.insert
        (Index.toInt <| Index.next index)
        Note.empty
        beat
        |> Beat


removeNote : Index (Note Encoding.None) -> Beat Encoding.None -> Beat Encoding.None
removeNote index (Beat beat) =
    beat
        |> ArrayUtil.remove (Index.toInt index)
        |> Beat


toString : Beat encoding -> String
toString (Beat beat) =
    beat
        |> Array.toList
        |> List.map Note.toString
        |> String.join noteDelimiter


fromString : String -> Beat encoding
fromString str =
    str
        |> String.split noteDelimiter
        |> List.map Note.fromString
        |> Array.fromList
        |> Beat


noteDelimiter : String
noteDelimiter =
    ";"


empty : Int -> Beat Encoding.None
empty thisLength =
    Array.repeat thisLength Note.empty
        |> Beat


setNote : Index (Note Encoding.None) -> Note Encoding.None -> Beat Encoding.None -> Beat Encoding.None
setNote index note (Beat beat) =
    Array.set (Index.toInt index) note beat
        |> Beat
