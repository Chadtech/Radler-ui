{-# LANGUAGE OverloadedStrings #-}


module Prelude.Extra 
    ( head
    , listMap2
    , List
    , (<<)
    , textToInt
    , debugLog
    , mapFirst
    ) where


import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Flow
import Prelude hiding (head)
import qualified Debug.Trace as Debug


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


infixl 0 <<
(<<) = leftCompose


leftCompose :: (b -> c) -> (a -> b) -> a -> c
leftCompose f g v =
    f (g v)


debugLog :: String -> (a -> String) -> a -> a
debugLog msg toString x =
    Debug.trace (msg ++ " : " ++ (toString x)) x


mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) =
    (f a, b)