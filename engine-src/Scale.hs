{-# LANGUAGE OverloadedStrings #-}


module Scale
    ( Scale
    , toFreq
    , Scale.read
    , Error
    , throw
    )
    where


import Data.Function ((&))
import Data.Text.Lazy (Text)
import Data.Text.Lazy as T
import Result (Result(Ok, Err))
import qualified Result
import qualified Scale.Major7ToneJit as Major7ToneJit


-- TYPES --


data Scale
    = Major7ToneJit


-- HELPERS --


read :: Text -> Result Error Scale
read txt =
    case txt of
        "major 7 tone jit" ->
            Ok Major7ToneJit

        _ ->
            Err (UnrecognizedScale txt)


toFreq :: Scale -> Text -> Result Error Float
toFreq scale =
    case scale of
        Major7ToneJit ->
            Result.mapError 
                Major7ToneJitError
                . Major7ToneJit.toFreq 


-- ERROR --


data Error
    = Major7ToneJitError Major7ToneJit.Error
    | UnrecognizedScale Text


throw :: Error -> Text
throw error =
    case error of
        Major7ToneJitError major7ToneJitError ->
            Major7ToneJit.throw major7ToneJitError

        UnrecognizedScale txt ->
            [ "This is not a recognized scale -> "
            , txt
            ]
                & T.concat

