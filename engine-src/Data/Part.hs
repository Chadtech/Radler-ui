module Data.Part 
    ( Model
    , Error
    , throw
    , readMany
    , file
    )
    where

import qualified Result
import Result (Result(Ok, Err))
import qualified Debug.Trace as Debug
import qualified Util
import Data.List.Split (splitOn)
import Data.List as List
import Flow
import qualified Util


-- TYPES --


data Model
    = Model
        { file :: String
        , length_ :: Int
        }


-- PUBLIC --


readMany :: String -> Result Error [ Model ]
readMany str =
    str
        |> splitOn ","
        |> List.map fromString
        |> Result.join


-- PRIVATE --


fromString :: String -> Result Error Model
fromString str =
    case splitOn ";" (Util.trim str) of
        fileName : lengthStr : [] ->
            fromTwoStrings fileName (Util.trim lengthStr)

        _ ->
            StringCantBeSplitInTwo str
                |> Err


fromTwoStrings :: String -> String -> Result Error Model
fromTwoStrings fileName lengthStr =
    case Util.readInt lengthStr of
        Just length_ ->
            Model
                { file = fileName
                , length_ = length_
                }
                |> Ok

        Nothing ->
            LengthCouldNotBeParsed lengthStr
                |> Err


-- ERROR --


data Error
    = LengthCouldNotBeParsed String
    | StringCantBeSplitInTwo String


throw :: Error -> String
throw error =
    "Part error -> \n    " ++ errorToString error


errorToString :: Error -> String
errorToString error =
    case error of
        StringCantBeSplitInTwo str ->
            "part string cant be split in two -> " ++ show str

        LengthCouldNotBeParsed str ->
            "part length could not be parsed -> " ++ show str

