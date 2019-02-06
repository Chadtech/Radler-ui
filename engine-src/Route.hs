{-# LANGUAGE OverloadedStrings #-}


module Route 
    ( Route(..)
    , decode
    ) where


import Flow

import Data.Function
import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T  
import Error (Error)
import qualified Error
import Score (Score)
import qualified Score


-- TYPES --


data Route 
    = Play (Either Error Score)
    | Build (Either Error Score)
    | Echo Text
    | Ping


instance Show Route where
    show route =
        case route of
            Play result ->
                "play " ++ show result

            Build result ->
                "build " ++ show result

            Echo text ->
                "echo " ++ T.unpack text

            Ping ->
                "ping"

-- HELPERS --


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

        
scoreRoute :: (Either Error Score -> Route) -> Text -> Maybe Route
scoreRoute routeCtor body =
    body
        |> Score.fromText
        |> Either.mapLeft Error.ScoreError
        |> routeCtor
        |> Just
