{-# LANGUAGE OverloadedStrings #-}


module Random
    ( Seed
    , Generator
    , float
    , generate
    , fromInt
    )
    where


import Flow
import Prelude.Extra
import qualified System.Random as Random


newtype Seed 
    = Seed Int


data Generator a
    = Generator (Seed -> (a, Seed)) 


fromInt :: Int -> Seed
fromInt =
    Seed


generate :: Seed -> Generator a -> (a, Seed)
generate seed (Generator f) =
    f seed


float :: Float -> Float -> Generator Float
float min_ max_ =
    Generator (floatInternal min_ max_)


floatInternal :: Float -> Float -> Seed -> (Float, Seed)
floatInternal min_ max_ seed =
    (min_, seed)