{-# LANGUAGE OverloadedStrings #-}


module Model 
    ( Model
    , name
    , init
    )
    where

import qualified Data.ByteString.Char8 as Char
import qualified Data.List as List
import Data.Note (Note)
import qualified Data.Note as Note
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Voice as Voice
import Parsing (Parser)
import Prelude hiding (init)
import Prelude.Extra (List)
import Error 
    ( Error
        ( VoiceError
        , ScoreError
        , UnexpectedChunkStructure
        )
    )
import Flow
import Result (Result(Ok, Err))
import qualified Result


data Model
    = Model
        { lastScoreData :: Text
        , name :: Text
        , voices :: List Voice.Model
        , score :: List (List Note)
        }


init :: Text -> Result Error Model
init scoreData =
    scoreData
        |> toChunks
        |> buildFromChunks scoreData


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
        

buildFromChunks :: Text -> List Text -> Result Error Model
buildFromChunks scoreData chunks =
    case chunks of
        name : voices : notes : [] ->
            Model 
                |> Ok
                |> applyText scoreData
                |> applyText name
                |> applyVoices voices
                |> applyScore notes

        [] ->
            Err UnexpectedChunkStructure


applyScore :: Text -> Parser Error (List (List Note)) b
applyScore txt ctorResult =
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

        