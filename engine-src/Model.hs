module Model 
    ( Model
    , name
    -- , project
    , fromScoreData
    )
    where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Part as Part
import qualified Data.Project as Project
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
import Result (Result(Ok, Err))
import qualified Result
import qualified Data.List as List
import qualified Data.ByteString.Char8 as Char
import Flow
import Error (Error(ProjectError, UnexpectedChunkStructure))
import Elmy (List)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

data Model
    = Model
        { lastScoreData :: Text
        , name :: Text
        }
        -- { -project :: Project.Model
        -- }


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




-- fromScoreString :: String -> Model
-- fromScoreString scoreStr =
--     Model
--         { lastScoreStr = scoreStr 
--         , name = "WOW"
--         }


getScoreString :: ByteString -> String
getScoreString scoreData = 
    scoreData
        |> Char.split '\n'
        |> List.map Char.unpack
        |> List.unlines