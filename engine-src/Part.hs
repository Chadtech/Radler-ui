{-# LANGUAGE OverloadedStrings #-}


module Part
    ( Part
    , build
    , diff    
    , toDevAudio
    , manyToDevAudio
    , Error
    , readMany
    , throw
    )
    where


import Flow
import Prelude.Extra

import Audio (Audio)
import qualified Audio
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Config (Config)
import qualified Config
import qualified Control.Monad as CM
import qualified Data.Either.Extra as Either
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Tuple.Extra as Tuple
import qualified Part.Sin as Sin
import qualified Part.Saw as Saw
import qualified Part.Osc as Osc
import Resolution (Resolution)
import qualified Resolution
import Room (Room)
import qualified Room 


-- TYPES --


data Part
    = Sin Osc.Model
    | Saw Osc.Model
    deriving (Eq)


instance Show Part where
    show part =
        case part of
            Sin model ->
                "Sin " ++ show model

            Saw model ->
                "Saw " ++ show model


-- HELPERS --


manyToDevAudio :: List Part -> Audio
manyToDevAudio parts =
    parts
        |> List.map toDevAudio
        |> Audio.normalizeVolumes
        |> Audio.mixMany


diff :: (Part, Part) -> Either Error (Resolution Part)
diff (incomingPart, existingPart) =
    case (incomingPart, existingPart) of
        (Sin incomingSinModel, Sin existingSinModel) ->
            Osc.diff incomingSinModel existingSinModel
                |> Either.mapRight (Resolution.map Sin) 
                |> Either.mapLeft SinError

        (Saw incomingSinModel, Saw existingSinModel) ->
            Osc.diff incomingSinModel existingSinModel
                |> Either.mapRight (Resolution.map Saw) 
                |> Either.mapLeft SawError

        _ ->
            Right Resolution.Unresolvable


fromPieces :: Config -> (Text, List Text) -> Either Error Part
fromPieces config (partTxt, noteTxts) =
    case T.splitOn "," partTxt of
        "sin" : partDetails ->
            noteTxts
                |> Osc.read 
                    config 
                    (Osc.Flags Sin.toMono partDetails)
                |> Either.mapRight Sin
                |> Either.mapLeft SinError

        "saw" : partDetails ->
            noteTxts
                |> Osc.read 
                    config 
                    (Osc.Flags Saw.toMono partDetails)
                |> Either.mapRight Saw
                |> Either.mapLeft SawError

        _ ->
            Left <| UnrecognizedPartType partTxt


readMany :: Config -> Text -> Text -> Either Error (List Part)
readMany config voiceNameTxts noteTxts =
    let
        voiceNames :: List Text
        voiceNames = 
            voiceNameTxts
                |> T.strip
                |> T.splitOn ";"

            
        notes :: List (List Text)
        notes =
            noteTxts
                |> T.strip
                |> T.splitOn "\n"
                |> List.map (T.splitOn ";")
                |> List.transpose
                |> List.reverse


        notesLength :: Int
        notesLength =
            List.length notes


        voicesLength :: Int
        voicesLength =
            List.length voiceNames
    in
    if notesLength == voicesLength then
        notes
            |> List.zip voiceNames
            |> List.map (fromPieces config)
            |> CM.sequence 

    else
        VoicesAndNotesNotOneToOne 
            voicesLength 
            notesLength
            |> Left


toDevAudio :: Part -> Audio
toDevAudio part =
    case part of
        Sin model ->
            model
                |> Osc.toMono
                |> Audio.fromMono

        Saw model ->
            model
                |> Osc.toMono
                |> Audio.fromMono


build :: Maybe Room -> Part -> Audio
build maybeRoom part =
    case part of
        Sin model ->
            model
                |> Osc.build maybeRoom

        Saw model ->
            model
                |> Osc.build maybeRoom


-- ERROR -- 


data Error
    = UnrecognizedPartType Text
    | VoicesAndNotesNotOneToOne Int Int
    | SinError Osc.Error
    | SawError Osc.Error
    | DiffError


throw :: Error -> Text
throw = 
    T.append "\nPart Error ->\n" 
        <. errorToText


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
                |> T.concat

        SinError sinError ->
            sinError
                |> Osc.throw 
                |> T.append "Error in Sin Voice -> \n" 

        SawError sawError ->
            sawError
                |> Osc.throw 
                |> T.append "Error in Saw Voice -> \n" 

        DiffError ->
            "Diffing parts that arent the same type"

