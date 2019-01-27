{-# LANGUAGE OverloadedStrings #-}


module Part.Sin
    ( Model
    , build
    , Part.Sin.read
    , toMono
    , Error
    , throw
    ) where


import Flow
import Prelude.Extra
        
import Audio (Audio)
import qualified Audio
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Audio.Mono.Position (positionMono)
import Config (Config)
import qualified Config
import qualified Data.Either.Extra as Either
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Note
import Parse (parse)
import qualified Parse
import qualified Part.Duration as Duration
import qualified Part.Volume as Volume
import Position (Position)
import qualified Position
import Room (Room)
import qualified Room 
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


read :: Config -> List Text -> List Text -> Either Error Model
read config detailsText = 
    Either.mapRight (readModel detailsText)
        <. readManyNoteTexts config


readModel :: List Text -> List Note -> Model
readModel detailsText notes =
    Model 
        (Vector.fromList notes)
        (applyPosition detailsText)


applyPosition :: List Text -> Maybe Position
applyPosition detailsText =
    case detailsText of
        first : rest ->
            case Parse.fields Parse.float first of
                Right fields ->
                    case Position.read fields of
                        Right position ->
                            Just position

                        Left _ ->
                            applyPosition rest

                Left _ ->
                    applyPosition rest

        [] ->
            Nothing


readManyNoteTexts :: Config -> List Text -> Either Error (List Note)
readManyNoteTexts config noteTexts =
    readManyNoteTextsAccumulate 
        config 
        noteTexts 
        []


readManyNoteTextsAccumulate :: Config -> List Text -> List Note -> Either Error (List Note)
readManyNoteTextsAccumulate config noteTexts notes =
    case noteTexts of
        first : rest ->
            case readNoteText config first of
                Right (Just note) ->
                    readManyNoteTextsAccumulate 
                        config 
                        rest 
                        (note : notes)

                Right Nothing ->
                    readManyNoteTextsAccumulate 
                        config 
                        rest 
                        notes

                Left err ->
                    Left err

        [] ->
            Right <| List.reverse notes


readNoteText :: Config -> Text -> Either Error (Maybe Note)
readNoteText config noteTxt =
    case Note.read config noteTxt of
        Right (noteBase, contentTxt) ->
            case contentTxt of
                "X" ->
                    Right Nothing

                _ ->
                    readNonEmptyNoteText
                        config
                        noteBase
                        contentTxt
                        |> Either.mapRight Just

        Left error ->
            Left <| NoteError error


readNonEmptyNoteText :: Config -> Note.Model -> Text -> Either Error Note
readNonEmptyNoteText config noteBase contentTxt =
    Note
        |> Right
        |> Parse.apply noteBase
        |> parse (Scale.toFreq (Config.scale config) (slice 0 2 contentTxt)) ScaleError
        |> parse (Volume.read (slice 4 6 contentTxt)) VolumeError
        |> parse (Duration.read config (slice 2 4 contentTxt)) DurationError


toMono :: Model -> Mono
toMono model = 
    model
        |> notes
        |> Vector.map noteToMono
        |> Mono.fromTimeline


noteToMono :: Note -> (Int, Mono)
noteToMono note =
    ( Note.time $ noteModel note
    , Mono.sin 
        (freq note)
        (duration note)
        |> Mono.setVolume (volume note)
        |> Mono.declip
    )


build :: Maybe Room -> Model -> Audio
build maybeRoom model = 
    let
        mono :: Mono
        mono =
            toMono model
    in
    case (maybeRoom, position model) of
        (Just room, Just position) ->
            positionMono room position mono
                |> Audio.fromStereo

        _ ->
            Audio.fromMono mono


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
                |> Scale.throw
                |> T.append "Scale Error ->\n"

        VolumeError volumeError ->
            Volume.throw volumeError

        DurationError durationError ->
            Duration.throw durationError