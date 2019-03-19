{-# LANGUAGE OverloadedStrings #-}


module Scale
    ( Scale
    , fromText
    , toFreq
    , Error
    , throw
    )
    where

        
import Flow

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import Data.Text.Lazy as T
import Freq (Freq)
import qualified Freq
import qualified Scale.Major7ToneJit as Major7ToneJit
import qualified Scale.Slendro as Slendro


-- TYPES --


data Scale
    = Major7ToneJit
    | Slendro
    deriving (Eq)


instance Show Scale where
    show scale =
        "Scale : " ++ (T.unpack <| toText scale)


-- HELPERS --


toText :: Scale -> Text
toText scale =
    case scale of
        Major7ToneJit ->
            "Scale : Major 7 Tone Jit"

        Slendro ->
            "Slendro"

                
fromText :: Text -> Either Error Scale
fromText txt =
    case txt of
        "major 7 tone jit" ->
            Right Major7ToneJit

        "slendro" ->
            Right Slendro

        _ ->
            Left <| UnrecognizedScale txt


toFreq :: Scale -> Text -> Either Error Freq
toFreq scale =
    case scale of
        Major7ToneJit ->
            Either.mapLeft
                Major7ToneJitError
                <. Major7ToneJit.toFreq 

        Slendro ->
            Either.mapLeft
                SlendroError
                <. Slendro.toFreq


-- ERROR --


data Error
    = Major7ToneJitError Major7ToneJit.Error
    | SlendroError Slendro.Error
    | UnrecognizedScale Text


instance Show Error where
    show error =
        T.unpack <| throw error


throw :: Error -> Text
throw error =
    case error of
        Major7ToneJitError major7ToneJitError ->
            major7ToneJitError
                |> Major7ToneJit.throw
                |> T.append "Major 7 Tone Jit Error ->\n"

        SlendroError slendroError ->
            slendroError
                |> Slendro.throw
                |> T.append "Slendro ->\n"

        UnrecognizedScale txt ->
            [ "This is not a recognized scale -> "
            , txt
            ]
                |> T.concat

