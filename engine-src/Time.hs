module Time
    ( Time(..)
    , fromInt
    , toInt
    ) where


-- TYPES --


newtype Time 
    = Time Int


-- HELPERS --


fromInt :: Int -> Time
fromInt =
    Time


toInt :: Time -> Int
toInt (Time int) =
    int