{-# LANGUAGE OverloadedStrings #-}


module Score
    ( Score
    , build
    , buildFilename
    , fromText
    , toDevAudio
    , devFilename
    , Error
    , throw
    )
    where

    
import Audio (Audio)
import qualified Audio
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Config (Config)
import qualified Config
import Data.Function ((&))
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (parse)
import Part (Part)
import qualified Part
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result


-- TYPES --


data Score
    = Score
        { name :: Text
        , parts :: List Part
        , config :: Config
        }
        deriving (Eq)


-- HELPERS --


buildFilename :: Score -> Int -> Text
buildFilename score partIndex =
    [ name score
    , "-"
    , T.pack $ show partIndex
    , ".wav"
    ]
        & T.concat


devFilename :: Score -> Text
devFilename score =
    T.append
        (name score)
        "-dev.wav"


fromText :: Text -> Result Error Score
fromText 
    = buildFromChunks 
    . toChunks 
     

toChunks :: Text -> List Text
toChunks 
    = T.splitOn ":"
    . T.unlines
    . List.filter isntCommentLine
    . T.splitOn "\n"
    . T.dropAround ((==) '"')


isntCommentLine :: Text -> Bool
isntCommentLine 
    = not
    . T.isPrefixOf "#"


buildFromChunks :: List Text -> Result Error Score
buildFromChunks chunks =
    case chunks of
        name : voices : notes : configTxt : [] ->
            case Config.read configTxt of
                Ok config ->
                    Score (T.strip name)
                        & Ok
                        & parse (Part.readMany config voices notes) PartError
                        & Result.apply config

                Err err ->
                    Err (ConfigError err)

        _ ->
            Err $ UnexpectedChunkStructure chunks
        

toDevAudio :: Score -> Audio
toDevAudio 
    = Audio.mixMany
    . Audio.normalizeVolumes
    . List.map Part.toDevAudio
    . parts


build :: Score -> List Audio
build 
    = Audio.normalizeVolumes
    . List.map Part.build
    . parts

-- ERROR --


data Error
    = PartError Part.Error
    | ConfigError Config.Error
    | UnexpectedChunkStructure (List Text)


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
                & T.concat

        ConfigError configError ->
            Config.throw configError