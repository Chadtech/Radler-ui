module Index 
    ( Index
    , fromInt
    ) where


newtype Index 
    = Index Int


fromInt :: Int -> Index
fromInt =
    Index