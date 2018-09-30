{-# LANGUAGE OverloadedStrings #-}


module Data.Route 
    ( Route(..)
    , decode
    ) where

import Data.Score (Score)
import Data.Text (Text)
import qualified Data.Text as T  
import Error (Error)
import Result (Result)



data Route 
    = Init (Result Error Score)
    | Ping



decode :: Text -> Text -> Maybe Route
decode routeTxt body =
    case routeTxt of
        "/init" ->
            Nothing

        "/ping" ->
            Just Ping