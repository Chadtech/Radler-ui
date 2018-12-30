{-# LANGUAGE OverloadedStrings #-}


module Part.Sin
    ( Model
    , Part.Sin.read
    , toMono
    , Error
    , throw
    ) where

        
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Config (Config)
import qualified Config
import Data.Function ((&))
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Note
import Parse (parse)
import qualified Part.Duration as Duration
import qualified Part.Volume as Volume
import Position (Position)
import qualified Position
import Prelude.Extra (List, slice)
import Result (Result(Ok, Err))
import qualified Result 
import Scale (Scale)
import qualified Scale


-- TYPES --


data Model
    = Model
        { notes :: Vector Note 
        , position :: Maybe Position
        }
        deriving (Eq)


data Note 
    = Note
        { noteModel :: Note.Model
        , freq :: Float
        , volume :: Float
        , duration :: Int
        }
        deriving (Eq)


-- HELPERS --


read :: Config -> List Text -> List Text -> Result Error Model
read config detailsText 
    = Result.andThen (readModel detailsText)
    . readManyNoteTexts config


readModel :: List Text -> List Note -> Result Error Model
readModel detailsText notes =
    Model (Vector.fromList notes)
        & Ok
        & Result.apply (applyPosition detailsText)


applyPosition :: List Text -> Maybe Position
applyPosition detailsText =
    case detailsText of
        first : rest ->
            case Position.read first of
                Ok position ->
                    Just position

                Err _ ->
                    applyPosition rest

        [] ->
            Nothing


readManyNoteTexts :: Config -> List Text -> Result Error (List Note)
readManyNoteTexts config noteTexts =
    readManyNoteTextsAccumulate 
        config 
        noteTexts 
        []


readManyNoteTextsAccumulate :: Config -> List Text -> List Note -> Result Error (List Note)
readManyNoteTextsAccumulate config noteTexts notes =
    case noteTexts of
        first : rest ->
            case readNoteText config first of
                Ok (Just note) ->
                    readManyNoteTextsAccumulate 
                        config 
                        rest 
                        (note : notes)

                Ok Nothing ->
                    readManyNoteTextsAccumulate 
                        config 
                        rest 
                        notes

                Err err ->
                    Err err

        [] ->
            Ok $ List.reverse notes


readNoteText :: Config -> Text -> Result Error (Maybe Note)
readNoteText config noteTxt =
    case Note.read config noteTxt of
        Ok (noteBase, contentTxt) ->
            case contentTxt of
                "X" ->
                    Ok Nothing

                _ ->
                    readNonEmptyNoteText
                        config
                        noteBase
                        contentTxt
                        & Result.map Just

        Err error ->
            Err $ NoteError error


readNonEmptyNoteText :: Config -> Note.Model -> Text -> Result Error Note
readNonEmptyNoteText config noteBase contentTxt =
    Note
        & Ok
        & Result.apply noteBase
        & parse (Scale.toFreq (Config.scale config) (slice 0 2 contentTxt)) ScaleError
        & parse (Volume.read (slice 4 6 contentTxt)) VolumeError
        & parse (Duration.read config (slice 2 4 contentTxt)) DurationError


toMono :: Model -> Mono
toMono 
    = Mono.fromTimeline
    . Vector.map noteToMono
    . notes


noteToMono :: Note -> (Int, Mono)
noteToMono note =
    ( Note.time 
        (noteModel note)
    , Mono.sin 
        (freq note)
        (duration note)
        & Mono.setVolume (volume note)
        & Mono.declip
    )



-- ERROR --


data Error 
    = NoteError Note.Error
    | ScaleError Scale.Error
    | VolumeError Volume.Error
    | DurationError Duration.Error


throw :: Error -> Text
throw error =
    case error of
        NoteError noteError ->
            Note.throw noteError

        ScaleError scaleError ->
            scaleError
                & Scale.throw
                & T.append "Scale Error ->\n"

        VolumeError volumeError ->
            Volume.throw volumeError

        DurationError durationError ->
            Duration.throw durationError