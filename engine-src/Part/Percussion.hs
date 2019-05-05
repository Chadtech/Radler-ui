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
import Data.Int (Int64)
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Tuple.Extra as Tuple
import Data.Vector (Vector)
import qualified Data.Vector as VectorPap
import Freq (Freq(Freq))
import qualified Freq
import qualified Mono.Fade
import qualified Note
import Parse (parse)
import qualified Parse
import Duration (Duration(Duration))
import qualified Duration
import Volume (Volume(Volume))
import qualified Volume
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
            |> show
            |> T.pack
        , osc
            |> position
            |> show
            |> T.pack
            |> T.append "Position : "
        ]
            |> T.unlines
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
        [ show <| volume note
        , " "
        , show <| seed note
        , " "
        , show <| sound note
        ]
            |> List.map T.pack
            |> T.concat
            |> T.unpack


data Flags
    = Flags 
        { fields :: Parse.Fields Text
        }


data Sound
    = Pulse Duration
    | Noise Duration
    | Hat Duration
    | Unit
    | Clunk
    | Kick
    deriving (Eq)


instance Show Sound where
    show sound = 
        T.unpack <| soundToText sound


-- HELPERS --


soundToText :: Sound -> Text
soundToText sound =
    case sound of
        Pulse _ ->
            "pulse"

        Noise _ ->
            "noise"

        Hat _ ->
           "hi-hat"

        Unit ->
            "unit"

        Clunk ->
            "clunk"

        Kick ->
            "kick"


makeFlags :: Parse.Fields Text -> Flags
makeFlags =
    Flags


diff :: Model -> Model -> Resolution Model
diff incomingModel existingModel =
    ( mapNotes 
        (Timeline.filterKey (isntNoteOf incomingModel)) 
        existingModel
    , mapNotes 
        (Timeline.filterKey (isntNoteOf existingModel)) 
        incomingModel
    )
        |> Resolution.Changes


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
        |> readManyNoteTexts config
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
        Right (time, seed, contentText) ->
            case contentText of
                "X" ->
                    Right Nothing

                "Q" ->
                    Right <| Just (time, HardRest)

                _ ->
                    readNonEmptyNoteText
                        config
                        (time, seed, contentText)
                        |> Either.mapRight Just

        Left error ->
            Left <| NoteError error


readNonEmptyNoteText :: Config -> (Time, Random.StdGen, Text) -> Either Error (Time, Note)
readNonEmptyNoteText config (time, seed, contentText) =
    let
        -- Something like "pul"
        noteText :: Text
        noteText =
            slice 0 3 contentText

        -- 00 to FF
        volumeText :: Text
        volumeText =
            slice 3 5 contentText

        -- Extra info for specific notes
        extraText :: Text
        extraText =
            slice 5 (fromInt64 <| T.length contentText) contentText

    in
    Note
        |> Right
        |> parse (Volume.read volumeText) VolumeError
        |> Parse.apply seed
        |> parse (readSoundText extraText noteText) id
        |> Either.mapRight ((,) time)


readSoundText :: Text -> Text -> Either Error Sound
readSoundText extraText soundText =
    case soundText of
        "pul" ->
            case Duration.read 10 extraText of
                Right duration ->
                    Right <| Pulse duration

                Left error ->
                    Left <| DurationError error

        "noi" ->
             case Duration.read 100 extraText of
                 Right duration ->
                     Right <| Noise  duration

                 Left error ->
                     Left <| DurationError error

        "hat" ->
             case Duration.read 100 extraText of
                 Right duration ->
                     Right <| Hat duration

                 Left error ->
                     Left <| DurationError error

        "cnk" ->
            Right Clunk

        "kic" ->
            Right Kick

        "unt" ->
            Right Unit

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
                Unit ->
                    Mono.singleton <| Volume.toFloat volume

                Pulse duration ->
                    volume
                        |> Volume.toFloat
                        |> Mono.fromSample duration
                        |> Mono.declip

                Noise (Duration duration) ->
                    let
                        noise :: List Float
                        noise =
                            Random.randomRs
                                (-1, 1)
                                seed
                    in
                    noise
                        |> List.take duration
                        |> Mono.fromList
                        |> Mono.setVolume volume
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.declip

                Hat (Duration duration) ->
                    let
                        noise :: Mono
                        noise =
                            Random.randomRs
                                (-1, 1)
                                seed
                                |> List.take duration
                                |> Mono.fromList
                                |> Mono.setVolume volume
                                |> Mono.Fade.out Timing.Linear
                    in
                    [ Mono.sin 0 (Freq 310) (Duration 1280)
                    , Mono.sin 0 (Freq 630) (Duration 1280)
                    , Mono.sin 0 (Freq 1510) (Duration 600)
                    ]
                        |> Mono.mixMany
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.deltaConvolve noise
                        |> Mono.declip

                Clunk ->
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
                            [ Mono.singleton 0.5
                            , Mono.silence (Duration 5)
                            , Mono.singleton 0.4
                            , Mono.silence (Duration 40)
                            , Mono.singleton 0.4
                            , Mono.silence (Duration 9)
                            , Mono.singleton 0.3
                            , Mono.silence (Duration 100)
                            , Mono.singleton 0.3
                            ]
                                |> Mono.concat
                                |> Mono.compress 1
                                |> Mono.setVolume (Volume 0.5)


                        (highFreq, seed1) =
                            ( Random.randomR
                                (423, 479)
                                seed
                            ) :: (Float, Random.StdGen)


                        (lowFreq, seed2) =
                            ( Random.randomR
                                (199, 221)
                                seed1
                            ) :: (Float, Random.StdGen)

                    in
                    [ pulseFadingIn
                    , Mono.sinByWaveCount 0 (Freq highFreq) 3
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.convolve pulseFadingIn
                        |> Mono.delay (Duration 25)

                    , Mono.sinByWaveCount 0 (Freq lowFreq) 3
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.convolve pulseFadingIn
                        |> Mono.delay (Duration 25)

                    ]
                        |> Mono.mixMany
                        |> Mono.setVolume (Volume 0.05)
                        |> Mono.compress 4
                        |> Mono.convolve drumBody
                        |> Mono.setVolume volume
                        |> Mono.declip

                Kick ->
                    let
                        pulseFadingIn :: Mono
                        pulseFadingIn =
                            Mono.fromSample
                                (Duration 10)
                                0.2
                                |> Mono.applyUntil
                                    25
                                    (Mono.Fade.in_ Timing.EaseInOut)


                        drumBody :: Mono
                        drumBody =
                            [ Mono.singleton 0.5
                            , Mono.silence (Duration 5)
                            , Mono.singleton 0.4
                            , Mono.silence (Duration 40)
                            , Mono.singleton 0.4
                            , Mono.silence (Duration 9)
                            , Mono.singleton 0.3
                            , Mono.silence (Duration 50)
                            , Mono.singleton 0.3
                            ]
                                |> Mono.concat

                        (highFreq, seed1) =
                            ( Random.randomR
                                (115, 129)
                                seed
                            ) :: (Float, Random.StdGen)


                        (lowFreq, seed2) =
                            ( Random.randomR
                                (36, 40)
                                seed1
                            ) :: (Float, Random.StdGen)
                    in
                    [ pulseFadingIn
                    , Mono.sinByWaveCount 0 (Freq highFreq) 3
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.deltaConvolve pulseFadingIn
                        |> Mono.compress 1
                    , Mono.sinByWaveCount 0 (Freq lowFreq) 3
                        |> Mono.Fade.out Timing.Linear
                        |> Mono.deltaConvolve pulseFadingIn
                        |> Mono.compress 1

                    ]
                        |> Mono.mixMany
                        |> Mono.compress 2
                        |> Mono.deltaConvolve drumBody
                        |> Mono.setVolume volume
                        |> Mono.declip


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
    | DurationError Duration.Error
    deriving (Eq)


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
            , "\nI was expecting something like \"pul80\""
            ]
                |> T.concat

        FieldsError text ->
            T.append "Fields Error -> " text

        DurationError error ->
            Duration.throw error