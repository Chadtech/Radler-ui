{-# LANGUAGE OverloadedStrings #-}


module Data.Voice 
    ( Model
    , Error
    , readMany
    , throw
    ) 
    where


import Data.Int (Int16)
import Data.Note (Note)
import Prelude.Extra (List)
import qualified Result
import Result (Result(Ok, Err))
import Flow
import Data.List as List
import Data.Text (Text)
import qualified Data.Text as T

-- TYPES -- 


data Model 
    = P
    | N


fromString :: Text -> Result Error Model
fromString txt =
    case T.strip txt of
        "p" ->
            Ok P

        "n" ->
            Ok N

        _ ->
            txt
                |> T.unpack
                |> UnrecognizedVoiceType
                |> Err


readMany :: Text -> Result Error (List Model)
readMany txt =
    txt
        |> T.splitOn ","
        |> List.map fromString
        |> Result.join


-- Error -- 


data Error
    = UnrecognizedVoiceType String


throw :: Error -> String
throw error =
    "Voice Error -> \n    " ++ errorToString error


errorToString :: Error -> String
errorToString error =
    case error of
        UnrecognizedVoiceType str ->
            "unrecognized voice type -> " ++ str