{-# LANGUAGE OverloadedStrings #-}


module Part.Osc
    ( Model
    , Note
    , Flags(..)
    , diff
    , build
    , notes
    , freq
    , duration
    , volume
    , Part.Osc.read
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
import qualified Data.Tuple.Extra as Tuple
import Data.Vector (Vector)
import qualified Data.Vector as VectorPap
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
import Time (Time)
import qualified Time
import Timeline (Timeline)
import qualified Timeline


-- TYPES --


data Model
    = Model
        { notes :: Timeline Note 
        , position :: Maybe Position
        , toMonoFromModel :: Note -> Mono
        }


instance Eq Model where
    (==) model0 model1 =
        (notes model0 == notes model1)
            && (position model0 == position model1)


instance Show Model where
    show osc =
        [ T.append
            "Number of Notes : " 
            (T.pack <| show <| Timeline.size <| notes osc)
        , T.append
            "Position : " 
            (T.pack <| show (position osc))
        ]
            |> T.concat
            |> T.unpack


data Note 
    = Note
        { noteModel :: Note.Model
        , freq :: Float
        , volume :: Float
        , duration :: Int
        }
        deriving (Eq)

    
instance Show Note where
    show note =
        "Note"


data Flags 
    = Flags
        { toMonoFromFlags :: Note -> Mono
        , detailsText :: List Text
        }



-- HELPERS --


diff :: Model -> Model -> Either Error (Resolution Model)
diff incomingModel existingModel =
    ( mapNotes 
        (Timeline.filterKey (isntNoteOf incomingModel)) 
        existingModel
    , mapNotes 
        (Timeline.filterKey (isntNoteOf existingModel)) 
        incomingModel
    )
        |> Resolution.Changes
        |> Right


isntNoteOf :: Model -> Time -> Note -> Bool
isntNoteOf model time note =
    case Timeline.get time (notes model) of
        Just modelsNote ->
            modelsNote /= note

        Nothing ->
            True


mapNotes :: (Timeline Note -> Timeline Note) -> Model -> Model
mapNotes f model =
    model { notes = f (notes model) }


sameLength :: Model -> Model -> Bool
sameLength incomingModel existingModel =
    Timeline.size (notes incomingModel) == Timeline.size (notes existingModel)


read :: Config -> Flags -> List Text -> Either Error Model
read config flags = 
    Either.mapRight (readModel flags)
        <. readManyNoteTexts config


readModel :: Flags -> List (Time, Note) -> Model
readModel flags notes =
    Model 
        (Timeline.fromList notes)
        (applyPosition <| detailsText flags)
        (toMonoFromFlags flags)


applyPosition :: List Text -> Maybe Position
applyPosition detailsText =
    case detailsText  of
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


readManyNoteTexts :: Config -> List Text -> Either Error (List (Time, Note))
readManyNoteTexts config noteTexts =
    readManyNoteTextsAccumulate 
        config 
        noteTexts 
        []


readManyNoteTextsAccumulate :: Config -> List Text -> List (Time, Note) -> Either Error (List (Time, Note))
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


readNoteText :: Config -> Text -> Either Error (Maybe (Time, Note))
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


readNonEmptyNoteText :: Config -> (Time, Note.Model) -> Text -> Either Error (Time, Note)
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
        |> Timeline.map (toMonoFromModel model)
        |> Timeline.toMono


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