{-# LANGUAGE OverloadedStrings #-}


module Part.Sin
    ( Model
    , Part.Sin.read
    , toAudio
    , Error
    , throw
    ) where

        
import Audio (Audio)
import qualified Audio
import Config (Config)
import qualified Config
import Data.Function ((&))
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Note
import Parse (Parser)
import qualified Parse
import qualified Part.Duration as Duration
import qualified Part.Volume as Volume
import Prelude.Extra (List, slice)
import Result (Result(Ok, Err))
import qualified Result 
import Scale (Scale)
import qualified Scale


-- TYPES --


data Model
    = Model
        { notes :: Vector Note }


data Note 
    = Note
        { noteModel :: Note.Model
        , freq :: Float
        , volume :: Float
        , duration :: Int
        }


-- HELPERS --


read :: Config -> List Text -> Result Error Model
read config 
    = Result.map (Model . Vector.fromList)
    . readManyNoteTexts config


readManyNoteTexts :: Config -> List Text -> Result Error (List Note)
readManyNoteTexts config noteTexts =
    readManyNoteTextsAccumulate config noteTexts []


readManyNoteTextsAccumulate :: Config -> List Text -> List Note -> Result Error (List Note)
readManyNoteTextsAccumulate config noteTexts notes =
    case noteTexts of
        first : rest ->
            case readNoteText config first of
                Ok (Just note) ->
                    readManyNoteTextsAccumulate config rest (note : notes)

                Ok Nothing ->
                    readManyNoteTextsAccumulate config rest notes

                Err err ->
                    Err err

        [] ->
            Ok (List.reverse notes)


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
            error
                & NoteError
                & Err

readNonEmptyNoteText :: Config -> Note.Model -> Text -> Result Error Note
readNonEmptyNoteText config noteBase contentTxt =
    Note
        & Ok
        & Parse.construct noteBase
        & applyFreq config (slice 0 2 contentTxt)
        & applyVolume (slice 2 4 contentTxt)
        & applyDuration config (slice 4 6 contentTxt)


applyDuration :: Config ->  Text -> Parser Error Int b
applyDuration config durationTxt resultCtor =
    case Duration.read config durationTxt of
        Ok duration ->
            Parse.construct duration resultCtor

        Err err ->
            Err (DurationError err)


applyVolume :: Text -> Parser Error Float b
applyVolume volumeTxt resultCtor =
    case Volume.read volumeTxt of
        Ok volume ->
            Parse.construct volume resultCtor

        Err err ->
            Err (VolumeError err)


applyFreq :: Config -> Text -> Parser Error Float b
applyFreq config noteTxt resultCtor =
    case Scale.toFreq (Config.scale config) noteTxt of
        Ok freq ->
            Parse.construct freq resultCtor

        Err err ->
            Err (ScaleError err)            


toAudio :: Config -> Model -> Audio
toAudio config model =
    model
        & notes
        & Vector.map (noteToAudio config)
        & Audio.fromTimeline


noteToAudio :: Config -> Note -> (Int, Audio)
noteToAudio config note =
    ( Note.time 
        (noteModel note)
    , Audio.sin 
        (freq note)
        (Config.beatLength config)
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