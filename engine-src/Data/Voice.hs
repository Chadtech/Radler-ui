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
import qualified Util

-- TYPES -- 


data Model 
    = P
    | N


fromPieces :: Text -> Result Error Model
fromPieces txt =
    case T.splitOn "," txt of
        "p" : [] ->
            Ok P

        "n" : [] ->
            Ok N

        _ ->
            txt
                |> T.unpack
                |> UnrecognizedVoiceType
                |> Err


readMany :: Text -> Result Error (List Model)
readMany txt =
    txt
        |> T.strip
        |> T.splitOn ";"
        |> List.map fromPieces
        |> Result.join


-- ERROR -- 


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