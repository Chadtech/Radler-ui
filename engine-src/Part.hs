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
import Mono (Mono)
import qualified Mono
import Config (Config)
import qualified Config
import qualified Control.Monad as CM
import qualified Data.Either.Extra as Either
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Tuple.Extra as Tuple
import Parse (parse)
import qualified Parse
import qualified Part.DullSaw as DullSaw
import qualified Part.Percussion as Percussion
import qualified Part.Harmonics as Harmonics
import qualified Part.Osc as Osc
import qualified Part.Sin as Sin
import qualified Part.Saw as Saw
import qualified Part.Test as Test
import Resolution (Resolution)
import qualified Resolution
import Room (Room)
import qualified Room 


-- TYPES --


data Part
    = Sin (Osc.Model Sin.Model)
    | Saw (Osc.Model Saw.Model)
    | Harmonics (Osc.Model Harmonics.Model)
    | DullSaw (Osc.Model DullSaw.Model)
    | Percussion Percussion.Model
    | Test Test.Model
    deriving (Eq)


instance Show Part where
    show part =
        case part of
            Sin model ->
                "Sin " ++ show model

            Saw model ->
                "Saw " ++ show model

            Harmonics model ->
                "Harmonics " ++ show model

            DullSaw model ->
                "Dull Saw " ++ show model

            Percussion model ->
                "Percussion " ++ show model

            Test model ->
                "Test" ++ show model


-- HELPERS --


manyToDevAudio :: List Part -> Audio
manyToDevAudio parts =
    parts
        |> List.map toDevAudio
        |> Audio.normalizeVolumes
        |> Audio.mixMany
        |> Audio.trimEnd


diff :: (Part, Part) -> Either Error (Resolution Part)
diff (incomingPart, existingPart) =
    case (incomingPart, existingPart) of
        (Sin incomingSinModel, Sin existingSinModel) ->
            Osc.diff incomingSinModel existingSinModel
                |> Resolution.map Sin
                |> Right

        (Saw incomingSawModel, Saw existingSawModel) ->
            Osc.diff incomingSawModel existingSawModel
                |> Resolution.map Saw
                |> Right

        (Harmonics incomingHarmonicsModel, Harmonics existingHarmonicsModel) ->
            Osc.diff incomingHarmonicsModel existingHarmonicsModel
                |> Resolution.map Harmonics
                |> Right

        (DullSaw incomingDullSawModel, DullSaw existingDullSawModel) ->
            Osc.diff incomingDullSawModel existingDullSawModel
                |> Resolution.map DullSaw
                |> Right

        (Percussion incomingPercussionModel, Percussion existingPercussionModel) ->
            Percussion.diff incomingPercussionModel existingPercussionModel
                |> Resolution.map Percussion
                |> Right

        (Test incomingTestModel, Test existingTestModel) ->
            Test.diff incomingTestModel existingTestModel
                |> Resolution.map Test
                |> Right

        _ ->
            Right Resolution.Unresolvable


fromPieces :: Config -> (Text, List Text) -> Either Error Part
fromPieces config (partTxt, noteTxts) =
    case nameAndFields partTxt of
        Right ("sin", fields) ->
            noteTxts
                |> Osc.read 
                    config 
                    (Sin.makeFlags fields)
                |> Either.mapRight Sin
                |> Either.mapLeft SinError

        Right ("saw", fields) ->
            noteTxts
                |> Osc.read 
                    config 
                    (Saw.makeFlags fields)
                |> Either.mapRight Saw
                |> Either.mapLeft SawError

        Right ("harmonics", fields) ->
            case Harmonics.makeFlags fields of
                Right flags ->
                    noteTxts
                        |> Osc.read config flags
                        |> Either.mapRight Harmonics
                        |> Either.mapLeft HarmonicsError

                Left error ->
                    Left <| HarmonicsFlagsError error

        Right ("dullsaw", fields) ->
            noteTxts
                |> Osc.read 
                    config 
                    (DullSaw.makeFlags fields)
                |> Either.mapRight DullSaw
                |> Either.mapLeft DullSawError

        Right ("percussion", fields) ->
            noteTxts
                |> Percussion.read
                    config 
                    (Percussion.makeFlags fields)
                |> Either.mapRight Percussion
                |> Either.mapLeft PercussionError


        Right ("test", fields) ->
            noteTxts
                |> Test.fromTexts config


        Left error ->
            Left error

        _ ->
            Left <| UnrecognizedPartType partTxt


nameAndFields :: Text -> Either Error (Text, Parse.Fields Text)
nameAndFields partText =
    case 
        partText
            |> T.filter ((/=) ' ')
            |> T.splitOn "|" 
    of
        name : rest : [] ->
            rest
                |> Parse.fromParameters
                |> Either.mapRight ((,) name)
                |> Either.mapLeft FieldsError

        _ ->
            Left VoiceInvalidFormat


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
            oscToDevAudio model

        Saw model ->
            oscToDevAudio model

        Harmonics model ->
            oscToDevAudio model

        DullSaw model ->
            oscToDevAudio model

        Percussion model ->
            model
                |> Percussion.toMono
                |> Audio.fromMono


oscToDevAudio :: Osc.Model t -> Audio 
oscToDevAudio =
    Osc.toMono .> Audio.fromMono


build :: Maybe Room -> Part -> Audio
build maybeRoom part =
    let
        build_ :: Osc.Model t -> Audio
        build_ =
            Osc.build maybeRoom .> Audio.trimEnd
    in
    case part of
        Sin model ->
            build_ model

        Saw model ->
            build_ model

        Harmonics model ->
            build_ model

        DullSaw model ->
            build_ model

        Percussion model ->
            model
                |> Percussion.build maybeRoom
                |> Audio.trimEnd


-- ERROR -- 


data Error
    = UnrecognizedPartType Text
    | VoicesAndNotesNotOneToOne Int Int
    | SinError Osc.Error
    | SawError Osc.Error
    | HarmonicsError Osc.Error
    | DullSawError Osc.Error
    | HarmonicsFlagsError Harmonics.Error
    | PercussionError Percussion.Error
    | DiffError
    | VoiceInvalidFormat
    | FieldsError Text


instance Show Error where
    show error = T.unpack <| throw error


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

        HarmonicsError harmonicsError ->
            harmonicsError
                |> Osc.throw 
                |> T.append "Error in Harmonics Voice -> \n" 

        DullSawError dullSawError ->
            dullSawError
                |> Osc.throw
                |> T.append "Error in Dull Saw Voice -> \n"

        PercussionError percussionError ->
            percussionError
                |> Percussion.throw
                |> T.append "Error in Percussion Voice -> \n"

        HarmonicsFlagsError harmonicsError ->
            harmonicsError
                |> Harmonics.throw
                |> T.append "Error making Harmonics Flags -> \n"

        DiffError ->
            "Diffing parts that arent the same type"

        VoiceInvalidFormat ->
            "The format of the voice was invalid. \
            \It should be something like -> \
            \ name | field(k=v) field(whatever)"

        FieldsError error ->
            [ "I was not able to parse the the fields in the voice.\ 
              \ The error was -> "
            , error
            ]
                |> T.concat

