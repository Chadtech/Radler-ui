{-# LANGUAGE OverloadedStrings #-}


module Config
    ( Config
    , Config.read
    , scale
    , beatLength
    , timingVariance
    , room
    , Error
    , throw
    )
    where


import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (parse)
import qualified Parse
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
    case T.splitOn ";" $ trimmedText of
        scale : beatLength : timingVariance : room : [] ->
            Config
                & Ok
                & parse (Scale.read scale) ScaleError
                & parse (Parse.decodeInt beatLength) BeatLengthIsntInt
                & parse (Parse.decodeInt timingVariance) TimingVarianceIsntInt
                & parse (Room.read room) RoomError

        _ ->
            Err $ UnexpectedConfigStructure trimmedText


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