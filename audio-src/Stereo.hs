{-# LANGUAGE OverloadedStrings #-}


module Stereo
    ( Stereo
    , append
    , fromList
    , trimEnd
    , fromMono
    , mix
    , toMonos
    , toMono 
    , toList   
    , fromMonos
    , setVolume
    , singleton
    , Stereo.subtract 
    , Stereo.length
    ) where


import Flow
import Prelude.Extra

import Mono (Mono)
import qualified Mono
import qualified Control.Monad as CM
import Data.Int (Int32)
import qualified Data.List as List
import qualified Data.Tuple.Extra as Tuple
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Vector
import Duration (Duration(Duration))
import Volume (Volume(..))
import qualified Volume


-- TYPES --


data Stereo
    = Stereo (Vector (Float, Float))
    deriving (Eq)


instance Show Stereo where
    show (Stereo stereo) =
        show stereo


-- HELPERS --


toList :: Stereo -> List (Float, Float)
toList (Stereo stereo) =
    Vector.toList stereo


fromList :: List (Float, Float) -> Stereo
fromList =
    Stereo <. Vector.fromList


singleton :: (Float, Float) -> Stereo
singleton sample =
    fromList [ sample ]


trimEnd :: Stereo -> Stereo
trimEnd (Stereo vector) =
    let
        isSampleZero :: Int -> (Float, Float) -> Maybe Int -> Maybe Int
        isSampleZero index sample maybeLastNonZeroIndex =
            if
                maybeLastNonZeroIndex == Nothing
                    && sample /= (0,0)
            then
                Just index

            else
                maybeLastNonZeroIndex
    in
    case
        Vector.ifoldr isSampleZero Nothing vector
    of
        Just lastNonZeroIndex ->
            Vector.take (lastNonZeroIndex + 1) vector
                |> Stereo

        Nothing ->
            Stereo vector


length :: Stereo -> Int
length (Stereo vector) =
    Vector.length vector


fromMono :: Mono -> Stereo
fromMono mono =
    mono
        |> Mono.toVector
        |> Vector.map pairSample
        |> Stereo


fromMonos :: (Mono, Mono) -> Stereo
fromMonos (!left, !right) =
    let
        (newLeft, newRight) =
            Mono.equalizeLengths
                left
                right
                |> Tuple.both Mono.toVector
    in
    Stereo <| Vector.zip newLeft newRight


pairSample :: Float -> (Float, Float)
pairSample sample =
    ( sample, sample )


silence :: Int -> Stereo
silence =
    fromMono <. Mono.silence <. Duration
    

subtract :: Stereo -> Stereo -> Stereo
subtract (Stereo stereoVector) =
    stereoVector
        |> Vector.map (Tuple.both ((*) (-1)))
        |> Stereo
        |> mix


toMonos :: Stereo -> (Mono, Mono)
toMonos (Stereo vector) =
    ( Mono.fromVector <| Vector.map fst vector
    , Mono.fromVector <| Vector.map snd vector
    )


toMono :: Stereo -> Mono
toMono (Stereo vector) =
    Vector.map combineAndHalf vector
        |> Mono.fromVector


combineAndHalf :: (Float, Float) -> Float
combineAndHalf (left, right) =
    (left + right) / 2


setVolume :: Volume -> Stereo -> Stereo
setVolume newRelativeVolume (Stereo vector) =
    let
        multiplyBothChannelsBy :: (Float, Float) -> (Float, Float)
        multiplyBothChannelsBy (leftSample, rightSample) =
            ( Volume.applyTo newRelativeVolume leftSample
            , Volume.applyTo newRelativeVolume rightSample
            )
    in
    Stereo 
        <| Vector.map 
            multiplyBothChannelsBy
            vector


mix :: Stereo -> Stereo -> Stereo
mix mono0 mono1 =
    let
        ( equalizedMono0, equalizedMono1 ) =
            equalizeLengths mono0 mono1
    in
    Stereo.zip equalizedMono0 equalizedMono1

            
zip :: Stereo -> Stereo -> Stereo
zip (Stereo vector0) (Stereo vector1) =
    Stereo <| Vector.zipWith addStereoSample vector0 vector1


addStereoSample :: (Float, Float) -> (Float, Float) -> (Float, Float)
addStereoSample ( l0, r0 ) ( l1, r1 ) =
    ( l0 + l1, r0 + r1 )


equalizeLengths :: Stereo -> Stereo -> (Stereo, Stereo)
equalizeLengths stereo0 stereo1 =
    let
        stereo0Length :: Int
        stereo0Length =
            Stereo.length stereo0

        stereo1Length :: Int
        stereo1Length =
            Stereo.length stereo1

    in
    if stereo0Length > stereo1Length then
        ( stereo0
        , appendSilence 
            (stereo0Length - stereo1Length) 
            stereo1
        )

    else
        ( appendSilence 
            (stereo1Length - stereo0Length) 
            stereo0
        , stereo1
        )


appendSilence :: Int -> Stereo -> Stereo
appendSilence duration stereo =
    append stereo (silence duration)


append :: Stereo -> Stereo -> Stereo
append (Stereo vector0) (Stereo vector1) =
    Stereo <| Vector.concat [ vector0, vector1 ]
