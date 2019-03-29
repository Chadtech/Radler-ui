{-# LANGUAGE OverloadedStrings #-}


module Timeline
    ( Timeline
    , size
    , get
    , Timeline.map
    , filterKey
    , fromList
    , toMono
    ) where


import Flow
import Prelude.Extra

import Mono (Mono)
import qualified Mono
import qualified Data.List as List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Tuple.Extra as Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Duration (Duration(Duration))
import Time (Time)
import qualified Time
import qualified Data.Vector.Unboxed as Unboxed


-- TYPES --


newtype Timeline a
    = Timeline (IntMap a)
    deriving (Eq)


instance Show a => Show (Timeline a) where
    show (Timeline intMap) =
        [ "Timeline "
        , T.pack <| show intMap
        ]
            |> T.concat
            |> T.unpack

-- HELPERS --


map :: (a -> b) -> Timeline a -> Timeline b
map f (Timeline intMap) =
    IntMap.map f intMap
        |> Timeline


size :: Timeline a -> Int
size (Timeline intMap) =
    IntMap.size intMap


get :: Time -> Timeline a -> Maybe a
get time (Timeline intMap) =
    IntMap.lookup (Time.toInt time) intMap


filterKey :: (Time -> a -> Bool) -> Timeline a -> Timeline a
filterKey condition (Timeline intMap) =
    intMap
        |> IntMap.filterWithKey (checkCondition condition)
        |> Timeline


checkCondition :: (Time -> a -> Bool) -> Int -> a -> Bool
checkCondition condition int item =
    condition (Time.fromInt int) item


fromList :: List (Time, a) -> Timeline a
fromList items =
    items
        |> List.map (Tuple.first Time.toInt)
        |> IntMap.fromList
        |> Timeline


toMono :: Timeline Mono -> Mono
toMono (Timeline intMap) =
    case IntMap.lookupMax intMap of
        Nothing ->
            Mono.empty

        Just lastMono ->
            let
                everySample :: Unboxed.Vector (Int, Float)
                everySample =
                    intMap
                        |> IntMap.mapWithKey timelineSamples
                        |> IntMap.elems
                        |> Unboxed.concat

            in
            everySample
                |> Unboxed.unsafeAccumulate (+) (timelineBasis everySample)
                |> Mono.fromVector


timelineLength :: Unboxed.Vector (Int, Float) -> Int
timelineLength =
    (+) 1 <. fst <. Unboxed.maximumBy laterSample


laterSample :: (Int, Float) -> (Int, Float) -> Ordering
laterSample (firstTime, _) (secondTime, _) =
    compare firstTime secondTime


timelineBasis :: Unboxed.Vector (Int, Float) -> Unboxed.Vector Float
timelineBasis everySample =
    everySample
        |> timelineLength
        |> Duration
        |> Mono.silence
        |> Mono.toVector


timelineSamples :: Int -> Mono -> Unboxed.Vector (Int, Float)
timelineSamples beginningTime mono =
    mono
        |> Mono.toVector
        |> Unboxed.indexed
        |> Unboxed.map (Tuple.first ((+) beginningTime))
