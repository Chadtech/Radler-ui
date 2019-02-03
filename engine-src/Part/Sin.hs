{-# LANGUAGE OverloadedStrings #-}


module Part.Sin
    ( Model
    , diff
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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
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
import Resolution (Resolution)
import qualified Resolution
import Room (Room)
import qualified Room 
import Scale (Scale)
import qualified Scale


-- TYPES --


data Model
    = Model
        { notes :: IntMap Note 
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


diff :: Model -> Model -> Either Error (Resolution Model)
diff incomingModel existingModel =
    ( mapNotes 
        (IntMap.filterWithKey (isntNoteOf incomingModel)) 
        existingModel
    , mapNotes 
        (IntMap.filterWithKey (isntNoteOf existingModel)) 
        incomingModel
    )
        |> Resolution.Changes
        |> Right


isntNoteOf :: Model -> Int -> Note -> Bool
isntNoteOf model key note =
    case IntMap.lookup key (notes model) of
        Just modelsNote ->
            modelsNote /= note

        Nothing ->
            True


mapNotes :: (IntMap Note -> IntMap Note) -> Model -> Model
mapNotes f model =
    model { notes = f (notes model) }


sameLength :: Model -> Model -> Bool
sameLength incomingModel existingModel =
    List.length (notes incomingModel) == List.length (notes existingModel)


read :: Config -> List Text -> List Text -> Either Error Model
read config detailsText = 
    Either.mapRight (readModel detailsText)
        <. readManyNoteTexts config


readModel :: List Text -> List (Int, Note) -> Model
readModel detailsText notes =
    Model 
        (IntMap.fromList notes)
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


readManyNoteTexts :: Config -> List Text -> Either Error (List (Int, Note))
readManyNoteTexts config noteTexts =
    readManyNoteTextsAccumulate 
        config 
        noteTexts 
        []


readManyNoteTextsAccumulate :: Config -> List Text -> List (Int, Note) -> Either Error (List (Int, Note))
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
            Right notes


readNoteText :: Config -> Text -> Either Error (Maybe (Int, Note))
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


readNonEmptyNoteText :: Config -> (Int, Note.Model) -> Text -> Either Error (Int, Note)
readNonEmptyNoteText config (time, noteBase) contentTxt =
    Note
        |> Right
        |> Parse.apply noteBase
        |> parse (Scale.toFreq (Config.scale config) (slice 0 2 contentTxt)) ScaleError
        |> parse (Volume.read (slice 4 6 contentTxt)) VolumeError
        |> parse (Duration.read config (slice 2 4 contentTxt)) DurationError
        |> Either.mapRight ((,) time)


toMono :: Model -> Mono
toMono model = 
    model
        |> notes
        |> IntMap.toList        
        |> Vector.fromList
        |> Vector.map noteToMono
        |> Mono.fromTimeline


noteToMono :: (Int, Note) -> (Int, Mono)
noteToMono (time, note) =
    ( time
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