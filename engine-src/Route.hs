{-# LANGUAGE OverloadedStrings #-}


module Route 
    ( Route(..)
    , decode
    ) where


import Data.Function
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T  
import Error (Error)
import qualified Error
import Result (Result)
import qualified Result
import Score (Score)
import qualified Score


data Route 
    = Play (Result Error Score)
    | Build (Result Error Score)
    | Echo Text
    | Ping


decode :: Text -> Text -> Maybe Route
decode routeTxt body =
    case routeTxt of
        "/play" ->
            scoreRoute Play body

        "/build" ->
            scoreRoute Build body

        "/ping" ->
            Just Ping

        "/echo" ->
            Just (Echo body)

        _ ->
            Nothing

        
scoreRoute :: (Result Error Score -> Route) -> Text -> Maybe Route
scoreRoute routeCtor 
    = Just
    . routeCtor
    . Result.mapError Error.ScoreError
    . Score.fromText
