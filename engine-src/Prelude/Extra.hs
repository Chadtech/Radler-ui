{-# LANGUAGE OverloadedStrings #-}


module Prelude.Extra 
    ( head
    , List
    , trace
    , mixLists
    , slice
    , toFloat
    , indexList
    , replaceChar
    ) where


import Flow

import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import Prelude hiding (head)
import qualified Debug.Trace as Debug


toFloat :: Int -> Float
toFloat =
    fromIntegral


slice :: Int -> Int -> Text -> Text
slice a b = 
    T.take (fromIntegral (b - a))
        <. T.drop (fromIntegral a)


head :: List a -> Maybe a
head list =
    case list of
        [] ->
            Nothing

        first : rest ->
            Just first


replaceChar :: Char -> Char -> Text -> Text
replaceChar target replacement = 
    T.pack 
        <. map (replaceIfTarget target replacement)
        <. T.unpack


replaceIfTarget :: Char -> Char -> Char -> Char
replaceIfTarget target replacement char =
    if char == target then
        replacement

    else
        char


indexList :: List a -> List (Int, a)
indexList xs =
    zipWith 
        (,) 
        ([ 0 .. List.length xs - 1 ]) 
        xs


mixLists :: List a -> List a -> List a
mixLists xs ys =
    case (xs, ys) of
        (firstX : restX, firstY : restY) ->
            firstX : firstY : mixLists restX restY

        (_, []) ->
            xs

        ([], _) ->
            ys


type List a = [ a ]


trace :: Show a => String -> a -> a
trace msg x =
    Debug.trace (msg ++ " : " ++ (show x)) x

