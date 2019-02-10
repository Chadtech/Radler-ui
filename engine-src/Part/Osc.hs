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


data Model t
    = Model
        { notes :: Timeline Note 
        , position :: Maybe Position
        , toMonoFromModel :: t -> Note -> Mono
        , subModel :: t
        }


instance Eq t => Eq (Model t) where
    (==) model0 model1 =
        (notes model0 == notes model1)
            && (position model0 == position model1)
            && (subModel model0 == subModel model1)


instance Show (Model t) where
    show osc =
        [ osc 
            |> notes
            |> Timeline.size
            |> show
            |> T.pack
            |> T.append "Number of Notes : " 
        , osc
            |> position
            |> show
            |> T.pack
            |> T.append "Position : "
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


data Flags t
    = Flags 
        { toMonoFromFlags :: t -> Note -> Mono
        , fields :: Parse.Fields Text
        , subModelFromFlags :: t
        }



-- HELPERS --


diff :: Model t -> Model t -> Either Error (Resolution (Model t))
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


isntNoteOf :: Model t -> Time -> Note -> Bool
isntNoteOf model time note =
    case Timeline.get time (notes model) of
        Just modelsNote ->
            modelsNote /= note

        Nothing ->
            True


mapNotes :: (Timeline Note -> Timeline Note) -> Model t -> Model t
mapNotes f model =
    model { notes = f (notes model) }


sameLength :: Model t -> Model t -> Bool
sameLength incomingModel existingModel =
    Timeline.size (notes incomingModel) == Timeline.size (notes existingModel)


read :: Config -> Flags t -> List Text -> Either Error (Model t)
read config flags noteTexts = 
    noteTexts
        |> (readManyNoteTexts config)
        |> andThen (readModel flags)

        
readModel :: Flags t -> List (Time, Note) -> Either Error (Model t)
readModel flags notes =
    case parsePosition <| fields flags of
        Right position ->
            Model 
                (Timeline.fromList notes)
                position
                (toMonoFromFlags flags)
                (subModelFromFlags flags)
                |> Right

        Left error ->
            Left error


parsePosition :: Parse.Fields Text -> Either Error (Maybe Position)
parsePosition fields =
    case Parse.get "position" fields of
        Nothing ->
            Right Nothing

        Just positionText ->
            case
                Parse.fromDelimitedText 
                    Parse.float
                    positionText
            of
                Right fields ->
                    fields
                        |> Position.read
                        |> Either.mapRight Just
                        |> Either.mapLeft PositionError

                Left error ->
                    Left <| FieldsError error


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


toMono :: Model t -> Mono
toMono model = 
    model
        |> notes
        |> Timeline.map 
            (toMonoFromModel model <| subModel model)
        |> Timeline.toMono


build :: Maybe Room -> Model t -> Audio
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
    | PositionError Position.Error
    | FieldsError Text


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

        PositionError positionError ->
            Position.throw positionError

        FieldsError text ->
            T.append "Fields Error -> " text