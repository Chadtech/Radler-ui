{-# LANGUAGE OverloadedStrings #-}


module Audio
    ( Audio
    , toVector
    , mixMany
    , normalizeVolumes
    , silence
    , Audio.sin
    ) where


import Data.Function ((&))
import Data.Int (Int16)
import qualified Data.List as List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude.Extra (List, debugLog)


data Audio
    = Audio (Vector Float)


toVector :: Audio -> Vector Float
toVector (Audio vector) =
    vector

    
empty :: Audio
empty =
    Audio Vector.empty


silence :: Int -> Audio
silence duration =
    Audio $ Vector.replicate duration 0


sin :: Float -> Int -> Audio
sin freq duration =
    Vector.generate 
        duration 
        (sinAtSample freq)
        & Audio


sinAtSample :: Float -> Int -> Float
sinAtSample freq index =
    Prelude.sin (2 * pi * (freq / 44100) * toFloat index) 


toFloat :: Int -> Float
toFloat =
    fromIntegral


mixMany :: List Audio -> Audio
mixMany audios =
    case audios of
        first : second : rest ->
            mix 
                (mix first second)
                (mixMany rest)

        first : [] ->
            first

        [] ->
            empty


normalizeVolumes :: List Audio -> List Audio
normalizeVolumes audios =
    List.map 
        (setVolume (1 / (debugLog "VOLUME" show (toFloat (List.length audios)))))
        audios


setVolume :: Float -> Audio -> Audio
setVolume newRelativeVolume (Audio vector) =
    Audio (Vector.map ((*) newRelativeVolume) vector)


mix :: Audio -> Audio -> Audio
mix audio0 audio1 =
    let
        ( equalizedAudio0, equalizedAudio1 ) =
            equalizeLengths audio0 audio1
    in
    Audio.zip equalizedAudio0 equalizedAudio1


zip :: Audio -> Audio -> Audio
zip (Audio vector0) (Audio vector1) =
    Audio $ Vector.zipWith (+) vector0 vector1


equalizeLengths :: Audio -> Audio -> (Audio, Audio)
equalizeLengths audio0 audio1 =
    let
        audio0Length :: Int
        audio0Length =
            Audio.length audio0

        audio1Length :: Int
        audio1Length =
            Audio.length audio1

    in
    if audio0Length > audio1Length then
        ( audio0
        , appendSilence (audio0Length - audio1Length) audio1
        )

    else
        ( appendSilence (audio1Length - audio0Length) audio0
        , audio1
        )


appendSilence :: Int -> Audio -> Audio
appendSilence duration audio =
    append audio (silence duration)


append :: Audio -> Audio -> Audio
append (Audio vector0) (Audio vector1) =
    Audio (Vector.concat [ vector0, vector1])


length :: Audio -> Int
length (Audio vector) =
    Vector.length vector

