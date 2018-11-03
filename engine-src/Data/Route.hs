{-# LANGUAGE OverloadedStrings #-}


module Data.Route 
    ( Route(..)
    , decode
    ) where


import Data.Function
import Data.Score (Score)
import qualified Data.Score as Score
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T  
import Error (Error)
import qualified Error
import Result (Result)
import qualified Result


data Route 
    = Play (Result Error Score)
    | Echo Text
    | Ping


decode :: Text -> Text -> Maybe Route
decode routeTxt body =
    case routeTxt of
        "/play" ->
            body
                & Score.fromText
                & Result.mapError Error.ScoreError
                & Play
                & Just

        "/ping" ->
            Just Ping

        "/echo" ->
            Just (Echo body)

        _ ->
            Nothing

        