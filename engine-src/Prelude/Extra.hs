{-# LANGUAGE OverloadedStrings #-}


module Prelude.Extra 
    ( andThen
    , List
    , trace
    , mapTrace
    , mixLists
    , slice
    , toFloat
    , replaceChar
    , showText
    ) where


import Flow

import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import Prelude hiding (head)
import qualified Debug.Trace as Debug


showText :: Show a => a -> Text
showText =
    show .> T.pack


andThen :: Monad m => (a -> m b) -> m a -> m b
andThen f m =
    m >>= f


toFloat :: Int -> Float
toFloat =
    fromIntegral


slice :: Int -> Int -> Text -> Text
slice a b = 
    T.take (fromIntegral (b - a))
        <. T.drop (fromIntegral a)


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
    mapTrace msg id x


mapTrace :: Show b => String -> (a -> b) -> a -> a
mapTrace msg f x =
    Debug.trace (msg ++ " : " ++ (show <| f x)) x