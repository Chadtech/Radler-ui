{-# LANGUAGE OverloadedStrings #-}


module Config
    ( Config
    , Config.read
    , scale
    , beatLength
    , timingVariance
    , Error
    , throw
    )
    where


import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (Parser)
import qualified Parse
import Prelude.Extra (textToInt)
import Result (Result(Ok, Err))
import qualified Result
import Room (Room)
import qualified Room
import Scale (Scale)
import qualified Scale


-- TYPES --


data Config
    = Config
        { scale :: Scale 
        , beatLength :: Int
        , timingVariance :: Int
        , room :: Maybe Room
        }
        deriving (Eq)


-- HELPERS --


read :: Text -> Result Error Config
read txt =
    let
        trimmedText :: Text
        trimmedText =
            T.strip txt
    in
    case T.splitOn ";" $ T.strip txt of
        scale : beatLength : timingVariance : room : [] ->
            Config
                & Ok
                & applyScale scale
                & applyBeatLength beatLength
                & applyTimingVariance timingVariance
                & applyRoom room

        _ ->
            UnexpectedConfigStructure trimmedText
                & Err


applyRoom :: Text -> Parser Error (Maybe Room) b
applyRoom text ctorResult =
    if text == "no-room" then
        Parse.construct Nothing ctorResult

    else
        case Room.read text of
            Ok room ->
                Parse.construct (Just room) ctorResult

            Err err ->
                Err $ RoomError err


applyTimingVariance :: Text -> Parser Error Int b
applyTimingVariance txt ctorResult =
    case textToInt txt of
        Just timingVariance ->
            Parse.construct timingVariance ctorResult

        Nothing ->
            Err (TimingVarianceIsntInt txt)


applyBeatLength :: Text -> Parser Error Int b
applyBeatLength txt ctorResult =
    case textToInt txt of
        Just beatLength ->
            Parse.construct beatLength ctorResult

        Nothing ->
            Err (BeatLengthIsntInt txt)


applyScale :: Text -> Parser Error Scale b
applyScale txt ctorResult =
    case Scale.read txt of
        Ok scale ->
            Parse.construct scale ctorResult

        Err err ->
            Err (ScaleError err)


-- ERROR --


data Error 
    = UnexpectedConfigStructure Text
    | BeatLengthIsntInt Text
    | TimingVarianceIsntInt Text
    | ScaleError Scale.Error
    | RoomError Room.Error


throw :: Error -> Text
throw error =
    case error of
        UnexpectedConfigStructure text ->
            [ "The structure of the config was not what I expected -> "
            , text
            ]
                & T.concat

        BeatLengthIsntInt text ->
            [ "This value isnt a valid beat length, its not an int ->"
            , text
            ]
                & T.concat

        TimingVarianceIsntInt text ->
            [ "This value isnt a valid timing variance, its not an int ->"
            , text
            ]
                & T.concat

        ScaleError scaleError ->
            Scale.throw scaleError

        RoomError roomError ->
            Room.throw roomError