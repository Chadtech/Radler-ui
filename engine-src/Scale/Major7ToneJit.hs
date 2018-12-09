{-# LANGUAGE OverloadedStrings #-}


module Scale.Major7ToneJit
    ( toFreq
    , Error
    , throw
    ) 
    where


import Data.Function ((&))
import Data.Text.Lazy (Text)
import Data.Text.Lazy as T
import Prelude.Extra (textToInt)
import Result (Result(Ok, Err))
import qualified Result
import Scale.Data (fundamental0)


toFreq :: Text -> Result Error Float
toFreq txt = 
    case textToInt txt of
        Just int ->
            int
                & fundamentalsTone
                & Result.map 
                    ((*) (fundamentalsOctave int))

        Nothing ->
            Err (NoteIsntInt txt)


fundamentalsOctave :: Int -> Float
fundamentalsOctave int =
    (2 ^ quot int 10) * fundamental0
        & Prelude.fromIntegral


fundamentalsTone :: Int -> Result Error Float
fundamentalsTone int =
    let
        toneInt :: Int
        toneInt =
            rem int 10
    in
    case toneInt of
        0 ->
            Ok 1

        1 ->
            Ok 1.125

        2 ->
            Ok 1.25

        3 ->
            Ok 1.333

        4 ->
            Ok 1.5

        5 ->
            Ok 1.667

        6 ->
            Ok 1.875

        _ ->
            Err (ToneIsntInScale toneInt)


-- ERROR --


data Error
    = NoteIsntInt Text
    | ToneIsntInScale Int


throw :: Error -> Text
throw error =
    case error of
        NoteIsntInt note ->
            [ "This note is weird..\n"
            , note
            , "\nIts not an integer. Im expecting something like 34 or 10"
            ]
                & T.concat

        ToneIsntInScale int ->
            [ "The Major 7 Tone Jit scale only goes from 0 to 6, but I got "
            , T.pack (show int)
            ]
                & T.concat

        