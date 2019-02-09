{-# LANGUAGE OverloadedStrings #-}


module Score
    ( Score
    , Resolution(..)
    , build
    , buildFilename
    , diff
    , fromText
    , toDevAudio
    , devFilename
    , numberOfParts
    , Error
    , throw
    )
    where


import Flow
import Prelude.Extra
    
import Audio (Audio)
import qualified Audio
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Config (Config)
import qualified Config
import qualified Control.Monad as CM
import qualified Data.Either.Extra as Either
import Data.List as List
import qualified Data.Traversable as Traversable
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Index (Index)
import qualified Index
import Parse (parse)
import qualified Parse
import Part (Part)
import qualified Part
import Resolution (Resolution)
import qualified Resolution
import Room (Room)
import qualified Room 


-- TYPES --


data Score
    = Score
        { name :: Text
        , parts :: List Part
        , config :: Config
        }
        deriving (Eq)


instance Show Score where
    show score =
        "score"


-- HELPERS --


diff :: Score -> Score -> Either Error (Resolution (List Part))
diff incomingScore existingScore =
    if incomingScore == existingScore then
        Right Resolution.Identical

    else if name incomingScore /= name existingScore then
        Right Resolution.Unresolvable
        
    else if config incomingScore /= config existingScore then
        Right Resolution.Unresolvable

    else if not <| sameNumberOfParts incomingScore existingScore then
        Right Resolution.Unresolvable

    else
        List.zip
            (parts incomingScore)
            (parts existingScore)
            |> List.map Part.diff
            |> CM.sequence
            |> Either.mapRight Traversable.sequenceA
            |> Either.mapLeft PartError


sameNumberOfParts :: Score -> Score -> Bool
sameNumberOfParts incomingScore existingScore =
    numberOfParts incomingScore == numberOfParts existingScore


numberOfParts :: Score -> Int
numberOfParts = 
    List.length <. parts


buildFilename :: Score -> Index -> Text
buildFilename score partIndex =
    [ name score
    , "-"
    , T.pack $ show partIndex
    , ".wav"
    ]
        |> T.concat


devFilename :: Score -> Text
devFilename score =
    T.append
        (name score)
        "-dev.wav"


fromText :: Text -> Either Error Score
fromText = 
    buildFromChunks <. toChunks 
     

toChunks :: Text -> List Text
toChunks scoreText = 
    scoreText
        |> T.dropAround ((==) '"')
        |> T.splitOn "\n"
        |> List.filter isntCommentLine
        |> T.unlines
        |> T.splitOn ":"


isntCommentLine :: Text -> Bool
isntCommentLine = 
    not <. T.isPrefixOf "#"


buildFromChunks :: List Text -> Either Error Score
buildFromChunks chunks =
    case chunks of
        name : voices : notes : configTxt : [] ->
            case Config.read configTxt of
                Right config ->
                    Score (T.strip name)
                        |> Right
                        |> parse (Part.readMany config voices notes) PartError
                        |> Parse.apply config

                Left err ->
                    Left <| ConfigError err

        _ ->
            Left <| UnexpectedChunkStructure chunks
        

toDevAudio :: Score -> Audio
toDevAudio score =
    score
        |> parts 
        |> Part.manyToDevAudio


build :: Score -> List Audio
build score =
    score 
        |> parts
        |> List.map 
            (Part.build <| Config.room <| config score)
        |> Audio.normalizeVolumes



-- ERROR --


data Error
    = PartError Part.Error
    | ConfigError Config.Error
    | UnexpectedChunkStructure (List Text)


instance Show Error where
    show = T.unpack <. throw 
    

throw :: Error -> Text
throw error =
    case error of
        PartError err ->
            Part.throw err

        UnexpectedChunkStructure chunks ->
            [ "I could not parse the score. \
                \The chunks werent what I expected. : ->\n\n"
            , T.intercalate "chunk\n\n" chunks
            ]
                |> T.concat

        ConfigError configError ->
            Config.throw configError