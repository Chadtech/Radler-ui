{-# LANGUAGE OverloadedStrings #-}


module Resolution
    ( Resolution(..)
    , Resolution.map 
    ) where


import Flow
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Tuple.Extra as Tuple


-- TYPES --


data Resolution a
    = Identical
    | Unresolvable
    -- Left a is the notes to remove
    -- Right b is the notes to add
    | Changes (a, a)
    deriving (Eq)



instance Show a => Show (Resolution a) where
    show resolution = 
        case resolution of
            Identical ->
                T.unpack "identical"
    
            Unresolvable ->
                T.unpack "unresolvable"
    
            Changes (remove, add) ->
                [ "Changes"
                , "Remove"
                , T.pack <| show remove
                , "Add"
                , T.pack <| show add
                ]
                    |> T.unlines
                    |> T.unpack


instance Functor Resolution where
    fmap f (Changes (x,y)) = Changes (f x, f y)
    fmap f Unresolvable = Unresolvable
    fmap f Identical = Identical


instance Applicative Resolution where
    pure a = Changes (a, a)
    Changes (f, g) <*> Changes (a, b) = Changes (f a, g b)
    _ <*> Identical = Identical
    _ <*> Unresolvable = Unresolvable
    Identical <*> _ = Identical
    Unresolvable <*> _ = Unresolvable


-- HELPERS --



map :: (a -> b) -> Resolution a -> Resolution b
map f resolution =
    case resolution of
        Identical ->
            Identical

        Unresolvable ->
            Unresolvable
        
        Changes tuple ->
            Tuple.both f tuple
                |> Changes
