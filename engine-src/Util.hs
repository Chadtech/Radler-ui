{-# LANGUAGE OverloadedStrings #-}


module Util 
    ( dropLast
    , readInt
    , log_
    )
    where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char
import Flow
import qualified Debug.Trace as Debug
import Text.Read (readMaybe)


isntLetter :: Char -> Bool
isntLetter c =
    not (Char.isLetter c)


readInt :: String -> Maybe Int
readInt =
    readMaybe


dropLast :: String -> String
dropLast str =
    case List.reverse str of
        _ : rest ->
            List.reverse rest

        [] ->
            str

-- Debug --


log_ :: String -> (a -> String) -> a -> a
log_ msg toString x =
    Debug.trace (msg ++ " : " ++ (toString x)) x