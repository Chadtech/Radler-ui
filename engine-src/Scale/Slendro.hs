{-# LANGUAGE OverloadedStrings #-}


module Scale.Slendro
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
            Right 1.142857

        2 ->
            Right 1.3125

        3 ->
            Right 1.5238

        4 ->
            Right 1.75

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
            [ "The Slendro scale only goes from 0 to 4, but I got "
            , T.pack (show int)
            ]
                |> T.concat

        