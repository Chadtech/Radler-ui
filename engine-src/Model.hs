{-# LANGUAGE OverloadedStrings #-}


module Model 
    ( Model
    , name
    , fromScoreData
    )
    where

import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
import qualified Data.ByteString.Char8 as Char
import qualified Data.List as List
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Voice as Voice
import Parsing (Parser)
import Prelude.Extra (List)
import Error (Error(VoiceError, UnexpectedChunkStructure))
import Flow
import Result (Result(Ok, Err))
import qualified Result


data Model
    = Model
        { lastScoreData :: Text
        , name :: Text
        , voices :: List Voice.Model
        }


fromScoreData :: ByteString -> Result Error Model
fromScoreData byteString =
    let
        scoreData =
            TE.decodeUtf8 byteString
    in
    scoreData
        |> T.splitOn "\n"
        |> List.filter isntCommentLine
        |> T.unlines
        |> T.splitOn ":"
        |> buildFromChunks scoreData


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

        [] ->
            Err UnexpectedChunkStructure


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

        