{-# LANGUAGE OverloadedStrings #-}


module Voice 
    ( Model
    , Error
    , readMany
    , throw
    ) 
    where

import Flow
import Prelude.Extra

import qualified Control.Monad as CM
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


-- TYPES -- 


data Model 
    = P
    | N
    | Sin


fromPieces :: Text -> Either Error Model
fromPieces txt =
    case T.splitOn "," txt of
        "p" : [] ->
            Right P

        "n" : [] ->
            Right N

        "sin" : [] ->
            Right Sin

        _ ->
            Left <| UnrecognizedVoiceType txt


readMany :: Text -> Either Error (List Model)
readMany voicesTxt = 
    voicesTxt
        |> T.strip
        |> T.splitOn ";"
        |> List.map fromPieces
        |> CM.sequence
        

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