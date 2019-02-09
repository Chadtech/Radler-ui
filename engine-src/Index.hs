module Index 
    ( Index
    , fromInt
    , list
    ) where


import Flow
import Prelude.Extra

import qualified Data.List as List
import qualified Data.Tuple.Extra as Tuple


newtype Index 
    = Index Int


instance Show Index where
    show (Index int) =
        "Index : " ++ show int


fromInt :: Int -> Index
fromInt =
    Index


list :: List a -> List (Index, a)
list list_ =
    zipWith 
        (,) 
        [ 0 .. List.length list_ - 1 ]
        list_
        |> List.map (Tuple.first fromInt)