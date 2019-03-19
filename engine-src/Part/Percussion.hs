{-# LANGUAGE OverloadedStrings #-}


module Part.Percussion
    ( Model
    , Note
    , Flags(..)
    , diff
    , build
    , notes
    , volume
    , makeFlags
    , Part.Percussion.read
    , toMono
    , Error
    , throw
    ) where


import Flow
import Prelude.Extra
        
import Audio (Audio)
import qualified Audio
import Mono (Mono)
import qualified Mono
import qualified Mono.Fade
import Mono.Position (positionMono)
import Config (Config)
import qualified Config
import Contour (Contour)
import qualified Contour
import qualified Data.Either.Extra as Either
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Tuple.Extra as Tuple
import Data.Vector (Vector)
import qualified Data.Vector as VectorPap
import Freq (Freq(Freq))
import qualified Freq
import qualified Note
import Parse (parse)
import qualified Parse
import Duration (Duration(Duration))
import qualified Duration
import Part.Volume (Volume)
import qualified Part.Volume as Volume
import Position (Position)
import qualified Position
import Resolution (Resolution)
import qualified Resolution
import Room (Room)
import qualified Room 
import qualified System.Random as Random
import Time (Time)
import qualified Time
import Timeline (Timeline)
import qualified Timeline
import qualified Timing


-- TYPES --


data Model
    = Model
        { notes :: Timeline Note 
        , position :: Maybe Position
        }


instance Eq Model where
    (==) model0 model1 =
        (notes model0 == notes model1)
            && (position model0 == position model1)


instance Show Model where
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
        { volume :: Volume
        , seed :: Random.StdGen
        , sound :: Sound
        }
    | HardRest


instance Eq Note where
    note0 == note1 =
        (volume note0 == volume note1)
        && (show (seed note0)) == (show (seed note1))
        && (sound note0) == (sound note1)

    
instance Show Note where
    show note =
        "Note"


data Flags
    = Flags 
        { fields :: Parse.Fields Text
        }


data Sound
    = Pulse
    | Kick
    deriving (Eq)


-- HELPERS --


makeFlags :: Parse.Fields Text -> Flags
makeFlags =
    Flags


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


read :: Config -> Flags -> List Text -> Either Error Model
read config flags noteTexts = 
    noteTexts
        |> (readManyNoteTexts config)
        |> andThen (readModel flags)


readModel :: Flags -> List (Time, Note) -> Either Error Model
readModel flags notes =
    Model 
        |> Right
        |> Parse.apply (Timeline.fromList notes)
        |> parse (parsePosition <| fields flags) id


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
        Right (time, seed, contentTxt) ->
            case contentTxt of
                "X" ->
                    Right Nothing

                "Q" ->
                    Right <| Just (time, HardRest)

                _ ->
                    readNonEmptyNoteText
                        config
                        (time, seed, contentTxt) 
                        |> Either.mapRight Just

        Left error ->
            Left <| NoteError error


readNonEmptyNoteText :: Config -> (Time, Random.StdGen, Text) -> Either Error (Time, Note)
readNonEmptyNoteText config (time, seed, contentTxt) =
    let
        -- Something like "pl"
        noteText :: Text
        noteText =
            slice 0 2 contentTxt

        -- 00 to FF
        volumeText :: Text
        volumeText =
            slice 2 4 contentTxt
        
    in
    Note
        |> Right
        |> parse (Volume.read volumeText) VolumeError
        |> Parse.apply seed
        |> parse (readSoundText noteText) id
        |> Either.mapRight ((,) time)


readSoundText :: Text -> Either Error Sound
readSoundText soundText =
    case soundText of
        "pl" ->
            Right Pulse

        "ki" ->
            Right Kick

        _ ->
            Left <| UnrecognizedSoundText soundText


toMono :: Model -> Mono
toMono model = 
    model
        |> notes
        |> Timeline.map noteToMono
        |> Timeline.toMono


noteToMono :: Note -> Mono
noteToMono note =
    case note of
        HardRest ->
            Mono.silence <| Duration (44100 * 30)

        Note volume seed sound ->
            case sound of
                Pulse ->
                    Mono.singleton <| Volume.toFloat volume

                Kick ->
                    let
                        pulseFadingIn :: Mono
                        pulseFadingIn =
                            Mono.fromSample 
                                (Duration 50)
                                0.2
                                |> Mono.applyUntil
                                    25
                                    (Mono.Fade.in_ Timing.EaseInOut)


                        drumBody :: Mono
                        drumBody =
                            [ Mono.singleton 1
                            , Mono.silence (Duration 2)
                            , Mono.singleton 0.8
                            , Mono.silence (Duration 5)
                            , Mono.singleton 0.66
                            ]
                                |> Mono.concat
                                |> Mono.convolve
                                    (Mono.sinByWaveCount 0 (Freq 10) 1)

                    in
                    [ pulseFadingIn
                    , Mono.sinByWaveCount 0 (Freq 451) 3
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.convolve pulseFadingIn
                    , Mono.sinByWaveCount 0 (Freq 210) 3
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.convolve pulseFadingIn
                    ]
                        |> Mono.mixMany
                        |> Mono.convolve drumBody
                        |> Mono.delay (Duration 10)


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
    | VolumeError Volume.Error
    | PositionError Position.Error
    | UnrecognizedSoundText Text
    | FieldsError Text


throw :: Error -> Text
throw error =
    case error of
        NoteError noteError ->
            Note.throw noteError

        VolumeError volumeError ->
            Volume.throw volumeError

        PositionError positionError ->
            Position.throw positionError

        UnrecognizedSoundText soundText ->
            [ "The following is not a valid sound type -> \n"
            , soundText
            , "\nI was expecting something like \"pl80\""
            ]
                |> T.concat


        FieldsError text ->
            T.append "Fields Error -> " text
