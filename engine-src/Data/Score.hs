{-# LANGUAGE OverloadedStrings #-}


module Data.Score
    ( Score
    , fromText
    )
    where

import Data.List as List
import Data.Note (Note)
import qualified Data.Note as Note
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Voice as Voice
import Flow
import Parsing (Parser)
import Prelude.Extra (List)
import Error 
    ( Error
        ( VoiceError
        , ScoreError
        , UnexpectedChunkStructure
        )
    )
import Result (Result(Ok, Err))
import qualified Result


data Score
    = Score
        { sourceText :: Text
        , name :: Text
        , voices :: List Voice.Model
        , notes :: List (List Note)
        }



fromText :: Text -> Result Error Score
fromText txt =
    txt
        |> toChunks
        |> buildFromChunks txt


toChunks :: Text -> List Text
toChunks bs =
    bs
        |> T.splitOn "\n"
        |> List.filter isntCommentLine
        |> T.unlines
        |> T.splitOn ":"


isntCommentLine :: Text -> Bool
isntCommentLine line =
    T.isPrefixOf "#" line 
        |> not


buildFromChunks :: Text -> List Text -> Result Error Score
buildFromChunks scoreData chunks =
    case chunks of
        name : voices : notes : [] ->
            Score
                |> Ok
                |> applyText scoreData
                |> applyText name
                |> applyVoices voices
                |> applyNotes notes

        [] ->
            Err UnexpectedChunkStructure


applyNotes :: Text -> Parser Error (List (List Note)) b
applyNotes txt ctorResult =
    case Note.readScore txt of
        Ok score ->
            Result.map 
                ((|>) score)
                ctorResult

        Err err ->
            Err (ScoreError err)


applyVoices :: Text -> Parser Error (List Voice.Model) b
applyVoices txt ctorResult =
    case Voice.readMany txt of
        Ok voices ->
            Result.map 
                ((|>) voices) 
                ctorResult

        Err err ->
            Err (VoiceError err)


applyText :: Text -> Parser Error Text b
applyText txt =
    Result.map ((|>) txt)
