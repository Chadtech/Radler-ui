{-# LANGUAGE OverloadedStrings #-}


module Data.Route 
    ( Route(..)
    ) where


import Result (Result)
import Data.Score (Score)
import Error (Error)


data Route 
    = Init (Result Error Score)
    | Ping