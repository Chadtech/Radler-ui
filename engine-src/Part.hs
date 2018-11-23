{-# LANGUAGE OverloadedStrings #-}


module Part
    ( Part
    , toAudio
    , Error
    , readMany
    , throw
    )
    where


import Audio (Audio)
import Config (Config)
import qualified Config
import qualified Audio
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


-- HELPERS --


fromPieces :: Config -> (Text, List Text) -> Result Error Part
fromPieces config (voiceNameTxt, noteTxts) =
    case T.splitOn "," voiceNameTxt of
        "sin" : [] ->
            noteTxts
                & Sin.read config
                & Result.map Sin
                & Result.mapError SinError

        _ ->
            Err (UnrecognizedPartType voiceNameTxt)


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


toAudio :: Part -> Audio
toAudio part =
    case part of
        Sin sinModel ->
            Sin.toAudio sinModel


-- ERROR -- 


data Error
    = UnrecognizedPartType Text
    | VoicesAndNotesNotOneToOne Int Int
    | SinError Sin.Error


throw :: Error -> Text
throw 
    = T.append "Part Error -> \n    " 
    . errorToText


errorToText :: Error -> Text
errorToText error =
    case error of
        UnrecognizedPartType txt ->
            T.append "unrecognized part type -> " txt

        VoicesAndNotesNotOneToOne voicesLength notesLength ->
            [ "There are " 
            , T.pack (show voicesLength)
            , "voices, but there are "
            , T.pack (show notesLength)
            , "lines of notes. They should be the same"
            ]
                & T.concat

        SinError sinError ->
            sinError
                & Sin.throw 
                & T.append "Error in Sin Voice -> " 