{-# LANGUAGE OverloadedStrings #-}


module Score
    ( Score
    , fromText
    , toAudio
    , name
    , Error
    , throw
    )
    where

        
import Audio (Audio)
import qualified Audio
import Config (Config)
import qualified Config
import Data.Function ((&))
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (Parser)
import qualified Parse
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


-- HELPERS --


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
                    Score
                        & Ok
                        & Parse.construct (T.strip name)
                        & applyParts config voices notes
                        & Parse.construct config

                Err err ->
                    Err (ConfigError err)

        _ ->
            Err (UnexpectedChunkStructure chunks)


applyParts :: Config -> Text -> Text -> Parser Error (List Part) b
applyParts config voices notes ctorResult =
    case Part.readMany config voices notes of
        Ok parts ->
            Parse.construct parts ctorResult

        Err err ->
            Err (PartError err)


toAudio :: Score -> Audio
toAudio 
    = Audio.mixMany
    . Audio.normalizeVolumes
    . List.map Part.toAudio
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