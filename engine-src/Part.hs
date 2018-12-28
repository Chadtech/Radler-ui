{-# LANGUAGE OverloadedStrings #-}


module Part
    ( Part
    , toDevAudio
    , build
    , Error
    , readMany
    , throw
    )
    where


import Audio (Audio)
import qualified Audio
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Config (Config)
import qualified Config
import Data.Function ((&))
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Part.Sin as Sin
import Prelude.Extra (List)
import qualified Result
import Result (Result(Ok, Err))


-- TYPES --


data Part
    = Sin Sin.Model
    deriving (Eq)


-- HELPERS --


fromPieces :: Config -> (Text, List Text) -> Result Error Part
fromPieces config (partTxt, noteTxts) =
    case T.splitOn "," partTxt of
        "sin" : partDetails ->
            noteTxts
                & Sin.read config partDetails
                & Result.map Sin
                & Result.mapError SinError

        _ ->
            Err $ UnrecognizedPartType partTxt


readMany :: Config -> Text -> Text -> Result Error (List Part)
readMany config voiceNameTxts noteTxts =
    let
        voiceNames :: List Text
        voiceNames 
            = T.splitOn ";" 
            $ T.strip
            $ voiceNameTxts

            
        notes :: List (List Text)
        notes 
            = List.reverse
            $ List.transpose
            $ List.map (T.splitOn ";") 
            $ T.splitOn "\n" 
            $ T.strip
            $ noteTxts


        notesLength :: Int
        notesLength =
            List.length notes


        voicesLength :: Int
        voicesLength =
            List.length voiceNames
    in
    if notesLength == voicesLength then
        notes
            & List.zip voiceNames
            & List.map (fromPieces config)
            & Result.join

    else
        VoicesAndNotesNotOneToOne 
            voicesLength 
            notesLength
            & Err


toDevAudio :: Part -> Audio
toDevAudio part =
    case part of
        Sin sinModel ->
            sinModel
                & Sin.toMono
                & Audio.fromMono


build :: Part -> Audio
build part =
    case part of
        Sin sinModel ->
            sinModel
                & Sin.toMono
                & Audio.fromMono


-- ERROR -- 


data Error
    = UnrecognizedPartType Text
    | VoicesAndNotesNotOneToOne Int Int
    | SinError Sin.Error


throw :: Error -> Text
throw 
    = T.append "\nPart Error ->\n" 
    . errorToText


errorToText :: Error -> Text
errorToText error =
    case error of
        UnrecognizedPartType txt ->
            T.append "unrecognized part type -> \n" txt

        VoicesAndNotesNotOneToOne voicesLength notesLength ->
            [ "There are " 
            , T.pack (show voicesLength)
            , " voice(s), but there are "
            , T.pack (show notesLength)
            , " line(s) of notes. They should be the same"
            ]
                & T.concat

        SinError sinError ->
            sinError
                & Sin.throw 
                & T.append "Error in Sin Voice -> \n" 

