{-# LANGUAGE OverloadedStrings #-}


module Voice 
    ( Model
    , Error
    , readMany
    , throw
    ) 
    where


import Prelude.Extra (List)
import qualified Result
import Result (Result(Ok, Err))
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


-- TYPES -- 


data Model 
    = P
    | N
    | Sin


fromPieces :: Text -> Result Error Model
fromPieces txt =
    case T.splitOn "," txt of
        "p" : [] ->
            Ok P

        "n" : [] ->
            Ok N

        "sin" : [] ->
            Ok Sin

        _ ->
            Err (UnrecognizedVoiceType txt)


readMany :: Text -> Result Error (List Model)
readMany 
    = Result.join
    . List.map fromPieces
    . T.splitOn ";"
    . T.strip


-- ERROR -- 


data Error
    = UnrecognizedVoiceType Text


throw :: Error -> Text
throw 
    = T.append "Voice Error -> \n    " 
    . errorToText


errorToText :: Error -> Text
errorToText error =
    case error of
        UnrecognizedVoiceType txt ->
            T.append "unrecognized voice type -> " txt