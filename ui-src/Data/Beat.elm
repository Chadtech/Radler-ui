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


toIndexedList : Beat encoding -> List ( Int, Note encoding )
toIndexedList (Beat beat) =
    Array.toIndexedList beat


fromList : List (Note encoding) -> Beat encoding
fromList =
    Array.fromList >> Beat


length : Beat encoding -> Int
length (Beat beat) =
    Array.length beat


addNoteAfter : Int -> Beat Encoding.None -> Beat Encoding.None
addNoteAfter index (Beat beat) =
    ArrayUtil.insert
        (index + 1)
        Note.empty
        beat
        |> Beat


removeNote : Int -> Beat Encoding.None -> Beat Encoding.None
removeNote index (Beat beat) =
    beat
        |> ArrayUtil.remove index
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


setNote : Int -> Note Encoding.None -> Beat Encoding.None -> Beat Encoding.None
setNote index note (Beat beat) =
    Array.set index note beat
        |> Beat
