{-# LANGUAGE OverloadedStrings #-}


module Scale
    ( Scale
    , toFreq
    , Scale.read
    , Error
    , throw
    )
    where

        
import Flow

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import Data.Text.Lazy as T
import qualified Scale.Major7ToneJit as Major7ToneJit


-- TYPES --


data Scale
    = Major7ToneJit
    deriving (Eq)


-- HELPERS --


read :: Text -> Either Error Scale
read txt =
    case txt of
        "major 7 tone jit" ->
            Right Major7ToneJit

        _ ->
            Left <| UnrecognizedScale txt


toFreq :: Scale -> Text -> Either Error Float
toFreq scale =
    case scale of
        Major7ToneJit ->
            Either.mapLeft
                Major7ToneJitError
                <. Major7ToneJit.toFreq 


-- ERROR --


data Error
    = Major7ToneJitError Major7ToneJit.Error
    | UnrecognizedScale Text


throw :: Error -> Text
throw error =
    case error of
        Major7ToneJitError major7ToneJitError ->
            major7ToneJitError
                |> Major7ToneJit.throw
                |> T.append "Major 7 Tone Jit Error ->\n"

        UnrecognizedScale txt ->
            [ "This is not a recognized scale -> "
            , txt
            ]
                |> T.concat

