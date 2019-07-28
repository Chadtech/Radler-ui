{-# LANGUAGE OverloadedStrings #-}


module Route 
    ( Route(..)
    , decode
    ) where


import Flow
import Prelude.Extra

import Data.Function
import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T  
import Error (Error)
import qualified Error
import Score (Score)
import qualified Score
import qualified Terminal


-- TYPES --


data Route 
    = Play (Either Error Score)
    | Build (Either Error Score)
    | Terminal (Either Error (List Terminal.Action))
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


decode :: Text -> Text -> IO (Maybe Route)
decode routeTxt body =
    return <| decodeHelp routeTxt body


decodeHelp :: Text -> Text -> Maybe Route
decodeHelp routeTxt body =
    case routeTxt of
        "/play" ->
            scoreRoute Play body

        "/build" ->
            scoreRoute Build body

        "/terminal" ->
            body
                |> Terminal.fromText
                |> Either.mapLeft Error.TerminalError
                |> Terminal
                |> Just

        "/ping" ->
            Just Ping

        "/echo" ->
            Just <| Echo body


        _ ->
            Nothing

        
scoreRoute :: (Either Error Score -> Route) -> Text -> Maybe Route
scoreRoute routeCtor body =
    body
        |> Score.fromText
        |> Either.mapLeft Error.ScoreError
        |> routeCtor
        |> Just
