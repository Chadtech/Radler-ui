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
import qualified Note
import Parse (Parser)
import qualified Parse
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result 
import Scale (Scale)
import qualified Scale


-- TYPES --


data Model
    = Model
        { notes :: List Note }


data Note 
    = Note
        { note :: Note.Model
        , freq :: Float
        }


-- HELPERS --


read :: Config -> List Text -> Result Error Model
read config 
    = Result.map Model
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
    case Note.read noteTxt of
        Ok (noteBase, contentTxt) ->
            case contentTxt of
                "X" ->
                    Ok Nothing

                _ ->
                    Note
                        & Ok
                        & Parse.construct noteBase
                        & applyFreq config contentTxt
                        & Result.map Just

        Err error ->
            error
                & NoteError
                & Err


applyFreq :: Config -> Text -> Parser Error Float b
applyFreq config noteTxt resultCtor =
    case Scale.toFreq (Config.scale config) noteTxt of
        Ok freq ->
            Parse.construct freq resultCtor

        Err err ->
            Err (ScaleError err)            


toAudio :: Config -> Model -> Audio
toAudio config model =
    Audio.sin 
        400
        (Config.beatLength config)


-- ERROR --


data Error 
    = NoteError Note.Error
    | ScaleError Scale.Error


throw :: Error -> Text
throw error =
    case error of
        NoteError noteError ->
            Note.throw noteError

        ScaleError scaleError ->
            Scale.throw scaleError