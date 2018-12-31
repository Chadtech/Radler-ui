{-# LANGUAGE OverloadedStrings #-}


module Prelude.Extra 
    ( head
    , listMap2
    , List
    , debugLog
    , mapFirst
    , mixLists
    , slice
    , toFloat
    , indexList
    , replaceChar
    ) where


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
slice a b 
    = T.take (fromIntegral (b - a))
    . T.drop (fromIntegral a)


head :: List a -> Maybe a
head list =
    case list of
        [] ->
            Nothing

        first : rest ->
            Just first


replaceChar :: Char -> Char -> Text -> Text
replaceChar target replacement 
    = T.pack 
    . map (replaceIfTarget target replacement)
    . T.unpack


replaceIfTarget :: Char -> Char -> Char -> Char
replaceIfTarget target replacement char =
    if char == target then
        replacement

    else
        char


indexList :: List a -> List (Int, a)
indexList xs =
    listMap2 
        (,) 
        ([ 0 .. List.length xs - 1 ]) 
        xs


listMap2 :: (a -> b -> c) -> List a -> List b -> List c
listMap2 f xs ys =
    listMap2Accumulate xs ys f []


listMap2Accumulate :: List a -> List b -> (a -> b -> c) -> List c -> List c
listMap2Accumulate xs ys f output =
    case (xs, ys) of
        (x : restXs, y : restYs) ->
            listMap2Accumulate 
                restXs 
                restYs 
                f 
                (f x y : output)

        _ ->
            List.reverse output


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


debugLog :: String -> (a -> String) -> a -> a
debugLog msg toString x =
    Debug.trace (msg ++ " : " ++ (toString x)) x


mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) =
    (f a, b)
    