{-# LANGUAGE OverloadedStrings #-}


module Prelude.Extra 
    ( List
    , require
    , andThen
    , fromInt64
    , fromInt32
    , trace
    , mapTrace
    , mixLists
    , slice
    , toFloat
    , replaceChar
    , showText
    , toIO
    , range
    , mark
    , mapIO
    , maxInt32Sample
    ) where


import Flow

import Data.Int (Int64, Int32)
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import Prelude hiding (head)
import qualified Debug.Trace as Debug


mapIO :: (a -> b) -> IO a -> IO b
mapIO =
    fmap


toIO :: a -> IO a
toIO =
    return

showText :: Show a => a -> Text
showText =
    show .> T.pack


andThen :: Monad m => (a -> m b) -> m a -> m b
andThen f m =
    m >>= f


require :: Monad m => m a -> m (a -> b) -> m b
require ma fm =
    andThen (\f -> fmap f ma) fm


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


mark :: String -> a -> a
mark =
    Debug.trace


range :: Int -> Int -> List Int
range n m =
    [n..m]


fromInt64 :: Int64 -> Int
fromInt64 =
    fromIntegral


fromInt32 :: Int32 -> Int
fromInt32 =
    fromIntegral


maxInt32Sample :: Float
maxInt32Sample =
    2147483647
