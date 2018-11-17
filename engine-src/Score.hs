{-# LANGUAGE OverloadedStrings #-}


module Score
    ( Score
    , fromText
    , toAudio
    , Error
    , throw
    )
    where

        
import Audio (Audio)
import qualified Audio
import Data.Function ((&))
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Voice as Voice
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
        { sourceText :: Text
        , name :: Text
        , parts :: List Part
        }


-- HELPERS --


fromText :: Text -> Result Error Score
fromText txt = 
    buildFromChunks txt $ toChunks txt
     

toChunks :: Text -> List Text
toChunks 
    = T.splitOn ":"
    . T.unlines
    . List.filter isntCommentLine
    . T.splitOn "\\n"
    . T.dropAround ((==) '"')


isntCommentLine :: Text -> Bool
isntCommentLine 
    = not
    . T.isPrefixOf "#"


buildFromChunks :: Text -> List Text -> Result Error Score
buildFromChunks scoreData chunks =
    case chunks of
        name : voices : notes : [] ->
            Score
                & Ok
                & Parse.construct scoreData
                & Parse.construct name
                & applyParts voices notes

        _ ->
            Err (UnexpectedChunkStructure chunks)



applyParts :: Text -> Text -> Parser Error (List Part) b
applyParts voices notes ctorResult =
    case Part.readMany voices notes of
        Ok parts ->
            Parse.construct parts ctorResult

        Err err ->
            Err (PartError err)


toAudio :: Score -> Audio
toAudio 
    = Audio.mixMany
    . List.map Part.toAudio
    . parts 


-- ERROR --


data Error
    = PartError Part.Error
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