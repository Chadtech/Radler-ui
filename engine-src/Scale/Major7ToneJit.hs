{-# LANGUAGE OverloadedStrings #-}


module Scale.Major7ToneJit
    ( toFreq
    , Error
    , throw
    ) 
    where


import Flow
import Prelude.Extra

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import Data.Text.Lazy as T
import Freq (Freq)
import qualified Freq
import qualified Parse
import qualified Constants


toFreq :: Text -> Either Error Freq
toFreq txt = 
    case Parse.decode Parse.int txt of
        Right int ->
            Either.mapRight 
                (Freq.fromFloat <. ((*) (fundamentalsOctave int)))
                (fundamentalsTone int) 

        Left _ ->
            Left <| NoteIsntInt txt



fundamentalsOctave :: Int -> Float
fundamentalsOctave int =
    (2 ^ quot int 10) * Constants.fundamental0
        |> toFloat


fundamentalsTone :: Int -> Either Error Float
fundamentalsTone int =
    let
        toneInt :: Int
        toneInt =
            rem int 10
    in
    case toneInt of
        0 ->
            Right 1

        1 ->
            Right 1.125

        2 ->
            Right 1.25

        3 ->
            Right 1.333

        4 ->
            Right 1.5

        5 ->
            Right 1.667

        6 ->
            Right 1.875

        _ ->
            Left <| ToneIsntInScale toneInt


-- ERROR --


data Error
    = NoteIsntInt Text
    | ToneIsntInScale Int
    deriving (Eq)


throw :: Error -> Text
throw error =
    case error of
        NoteIsntInt note ->
            [ "This note is weird..\n"
            , note
            , "\nIts not an integer. Im expecting something like 34 or 10"
            ]
                |> T.concat

        ToneIsntInScale int ->
            [ "The Major 7 Tone Jit scale only goes from 0 to 6, but I got "
            , T.pack (show int)
            ]
                |> T.concat

        