{-# LANGUAGE OverloadedStrings #-}


module Random
    ( Seed
    , fromInt
    )
    where


import Flow
import Prelude.Extra


newtype Seed 
    = Seed Int


fromInt :: Int -> Seed
fromInt =
    Seed