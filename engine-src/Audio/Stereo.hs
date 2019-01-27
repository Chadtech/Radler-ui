{-# LANGUAGE OverloadedStrings #-}


module Audio.Stereo
    ( Stereo
    , fromMono
    , mix
    , toMonos
    , fromMonos
    , setVolume
    , Audio.Stereo.length
    ) where


import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Cmd (Cmd)
import qualified Cmd
import qualified Control.Monad as CM
import Data.Function ((&))
import Data.Int (Int32)
import qualified Data.List as List
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Vector
import Prelude.Extra (List)


data Stereo
    = Stereo (Vector (Float, Float))


length :: Stereo -> Int
length (Stereo vector) =
    Vector.length vector


fromMono :: Mono -> Stereo
fromMono mono =
    mono
        & Mono.toVector
        & Vector.map pairSample
        & Stereo


fromMonos :: (Mono, Mono) -> Stereo
fromMonos (!left, !right) =
    Vector.zip 
        (Mono.toVector left)
        (Mono.toVector right)
        & Stereo


pairSample :: Float -> (Float, Float)
pairSample sample =
    ( sample, sample )


silence :: Int -> Stereo
silence =
    fromMono . Mono.silence
    

toMonos :: Stereo -> (Mono, Mono)
toMonos (Stereo vector) =
    ( Mono.fromVector $ Vector.map fst vector
    , Mono.fromVector $ Vector.map snd vector
    )


setVolume :: Float -> Stereo -> Stereo
setVolume newRelativeVolume (Stereo vector) =
    Stereo 
        $ Vector.map 
            (multiplyBothChannelsBy newRelativeVolume)
            vector


multiplyBothChannelsBy :: Float -> (Float, Float) -> (Float, Float)
multiplyBothChannelsBy newRelativeVolume (leftSample, rightSample) =
    ( newRelativeVolume * leftSample
    , newRelativeVolume * rightSample
    )


mix :: Stereo -> Stereo -> Stereo
mix mono0 mono1 =
    let
        ( equalizedMono0, equalizedMono1 ) =
            equalizeLengths mono0 mono1
    in
    Audio.Stereo.zip equalizedMono0 equalizedMono1

            
zip :: Stereo -> Stereo -> Stereo
zip (Stereo vector0) (Stereo vector1) =
    Stereo $ Vector.zipWith addStereoSample vector0 vector1


addStereoSample :: (Float, Float) -> (Float, Float) -> (Float, Float)
addStereoSample ( l0, r0 ) ( l1, r1 ) =
    ( l0 + l1, r0 + r1 )


equalizeLengths :: Stereo -> Stereo -> (Stereo, Stereo)
equalizeLengths stereo0 stereo1 =
    let
        stereo0Length :: Int
        stereo0Length =
            Audio.Stereo.length stereo0

        stereo1Length :: Int
        stereo1Length =
            Audio.Stereo.length stereo1

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
    Stereo $ Vector.concat [ vector0, vector1 ]
