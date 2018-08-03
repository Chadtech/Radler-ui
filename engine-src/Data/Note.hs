module Data.Note (Note) where


data Note =
    Note
    { length :: Int
    , volume :: Float
    , decay :: Int
    , note :: String
    }