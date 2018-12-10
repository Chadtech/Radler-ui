{-# LANGUAGE OverloadedStrings #-}


module Prelude.Extra 
    ( head
    , listMap2
    , List
    , textToInt
    , debugLog
    , mapFirst
    , slice
    ) where


import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import Prelude hiding (head)
import qualified Debug.Trace as Debug


slice :: Int -> Int -> Text -> Text
slice a b 
    = T.take (fromIntegral (b - a))
    . T.drop (fromIntegral a)


textToInt :: Text -> Maybe Int
textToInt txt =
    case TR.signed TR.decimal txt of
        Right (int, _) ->
            Just int

        Left _ ->
            Nothing


head :: List a -> Maybe a
head list =
    case list of
        [] ->
            Nothing

        first : rest ->
            Just first


listMap2 :: List a -> List b -> (a -> b -> c) -> List c
listMap2 xs ys f =
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


type List a = [ a ]


debugLog :: String -> (a -> String) -> a -> a
debugLog msg toString x =
    Debug.trace (msg ++ " : " ++ (toString x)) x


mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) =
    (f a, b)
    