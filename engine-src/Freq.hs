{-# LANGUAGE OverloadedStrings #-}

module Freq
    ( Freq(..)
    , fromFloat
    , applyTo
    , Freq.map
    ) where


newtype Freq
    = Freq Float
    deriving (Eq)


fromFloat :: Float -> Freq
fromFloat =
    Freq


map :: (Float -> Float) -> Freq -> Freq
map f (Freq fl) =
    Freq (f fl)


applyTo :: Freq -> Float -> Float
applyTo (Freq freq) fl =
    freq * fl