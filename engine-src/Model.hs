{-# LANGUAGE OverloadedStrings #-}


module Model 
    ( Model
    , name
    -- , project
    , fromScoreData
    )
    where

import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
import qualified Data.ByteString.Char8 as Char
import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Part as Part
import qualified Data.Project as Project
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Prelude.Extra (List)
import Error (Error(ProjectError, UnexpectedChunkStructure))
import Flow
import Result (Result(Ok, Err))
import qualified Result


data Model
    = Model
        { lastScoreData :: Text
        , name :: Text
        }


fromScoreData :: ByteString -> Result Error Model
fromScoreData byteString =
    let
        scoreData =
            TE.decodeASCII byteString
    in
    scoreData
        |> T.splitOn "\n"
        |> List.filter isntCommentLine
        |> T.unlines
        |> T.splitOn ":"
        |> buildFromChunks scoreData


isntCommentLine :: Text -> Bool
isntCommentLine =
    T.isPrefixOf "#"
        

buildFromChunks :: Text -> List Text -> Result Error Model
buildFromChunks scoreData chunks =
    case chunks of
        name : rest ->
            Model 
                { lastScoreData = scoreData
                , name = name
                }
                |> Ok

        [] ->
            Err UnexpectedChunkStructure

