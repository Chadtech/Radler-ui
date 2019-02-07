module Time
    ( Time(..)
    , fromInt
    , toInt
    ) where


newtype Time 
    = Time Int


fromInt :: Int -> Time
fromInt =
    Time


toInt :: Time -> Int
toInt (Time int) =
    int