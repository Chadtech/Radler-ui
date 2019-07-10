{-# LANGUAGE OverloadedStrings #-}

module Freq
  ( Freq(..)
  , fromFloat
  , toFloat
  , applyTo
  , Freq.map
  )
where

import           Flow


import           Data.Text.Lazy                 ( Text )
import           Data.Text.Lazy                as T


-- TYPES --


newtype Freq
    = Freq Float
    deriving (Eq)


instance Show Freq where
  show (Freq fl) = ["Freq ", T.pack <| show fl] |> T.concat |> T.unpack


-- HELPERS --


toFloat :: Freq -> Float
toFloat (Freq freq) = freq


fromFloat :: Float -> Freq
fromFloat = Freq


map :: (Float -> Float) -> Freq -> Freq
map f (Freq fl) = Freq (f fl)


applyTo :: Freq -> Float -> Float
applyTo (Freq freq) fl = freq * fl
