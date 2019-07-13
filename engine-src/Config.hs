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


import Flow

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (parse)
import qualified Parse
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


instance Show Config where
    show config =
        [ "Config : "
        , T.pack <| show (beatLength config)
        , T.pack <| show (timingVariance config)
        , T.pack <| show (room config)
        , T.pack <| show (scale config)
        ]
            |> T.concat
            |> T.unpack


-- HELPERS --


read :: Text -> Either Error Config
read txt =
    let
        trimmedText :: Text
        trimmedText =
            T.strip txt
    in
    case T.splitOn ";" <| trimmedText of
        scale : beatLength : timingVariance : room : [] ->
            Config
                |> Right
                |> parse (Scale.fromText scale) ScaleError
                |> parse (Parse.decodeInt beatLength) BeatLengthIsntInt
                |> parse (Parse.decodeInt timingVariance) TimingVarianceIsntInt
                |> parse (Room.maybeFromText room) RoomError

        _ ->
            Left <| UnexpectedConfigStructure trimmedText


-- ERROR --


data Error 
    = UnexpectedConfigStructure Text
    | BeatLengthIsntInt Text
    | TimingVarianceIsntInt Text
    | ScaleError Scale.Error
    | RoomError Room.Error
    deriving (Eq)


instance Show Error where
    show error =
        T.unpack <| throw error 

        
throw :: Error -> Text
throw error =
    case error of
        UnexpectedConfigStructure text ->
            [ "The structure of the config was not what I expected -> "
            , text
            ]
                |> T.concat

        BeatLengthIsntInt text ->
            [ "This value isnt a valid beat length, its not an int ->"
            , text
            ]
                |> T.concat

        TimingVarianceIsntInt text ->
            [ "This value isnt a valid timing variance, its not an int ->"
            , text
            ]
                |> T.concat

        ScaleError scaleError ->
            Scale.throw scaleError

        RoomError roomError ->
            Room.throw roomError