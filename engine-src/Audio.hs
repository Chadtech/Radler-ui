{-# LANGUAGE OverloadedStrings #-}


module Audio
    ( Audio
    , toVector
    , mixMany
    , silence
    ) where


import Data.Int (Int32)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude.Extra (List)


data Audio
    = Audio (Vector Int32)


toVector :: Audio -> Vector Int32
toVector (Audio vector) =
    vector

    
empty :: Audio
empty =
    Audio Vector.empty


silence :: Int -> Audio
silence duration =
    Audio $ Vector.replicate duration 0


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

