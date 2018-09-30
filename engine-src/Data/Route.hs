{-# LANGUAGE OverloadedStrings #-}


module Data.Route 
    ( Route(..)
    , decode
    ) where

import Data.Score (Score)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T  
import Error (Error)
import Result (Result)



data Route 
    = Init (Result Error Score)
    | Echo Text
    | Ping



decode :: Text -> Text -> Maybe Route
decode routeTxt body =
    case routeTxt of
        "/init" ->
            Nothing

        "/ping" ->
            Just Ping

        "/echo" ->
            Just (Echo body)

        _ ->
            Nothing

        