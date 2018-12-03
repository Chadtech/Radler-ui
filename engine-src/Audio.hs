{-# LANGUAGE OverloadedStrings #-}


module Audio
    ( Audio
    , toVector
    , mixMany
    , normalizeVolumes
    , silence
    , fromTimeline
    , write
    , play
    , Audio.sin
    ) where


import Cmd (Cmd)
import qualified Cmd
import qualified Control.Monad as CM
import Data.Function ((&))
import Data.Int (Int32)
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.WAVE (WAVE)
import qualified Data.WAVE as W
import Prelude.Extra (List, mapFirst)
import qualified System.Process as SP


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
        (setVolume (1 / (toFloat (List.length audios))))
        audios


setVolume :: Float -> Audio -> Audio
setVolume newRelativeVolume (Audio vector) =
    Audio (Vector.map ((*) newRelativeVolume) vector)


fromTimeline :: Vector (Int, Audio) -> Audio
fromTimeline timeline =
    if Vector.length timeline == 0 then
        empty

    else
        Vector.accumulate 
            (+)
            (timelineBasis $ Vector.last timeline)
            (CM.join $ Vector.map timelineSamples timeline)
            & Audio

timelineBasis :: (Int, Audio) -> Vector Float 
timelineBasis (lastStartingPoint, lastAudio) =
    lastStartingPoint + Audio.length lastAudio
        & silence
        & toVector



timelineSamples :: (Int, Audio) -> Vector (Int, Float)
timelineSamples (beginningIndex, Audio vector) =
    vector
        & Vector.indexed
        & Vector.map (mapFirst ((+) beginningIndex))





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
        , appendSilence 
            (audio0Length - audio1Length) 
            audio1
        )

    else
        ( appendSilence 
            (audio1Length - audio0Length) 
            audio0
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


toSamples :: Audio -> List Int32
toSamples (Audio vector) =
    vector
        & Vector.toList
        & List.map 
            (toInt32 . (*) 2147483647)


toInt32 :: Float -> Int32
toInt32 =
    round
    

write :: Text -> Audio -> Cmd
write fn audio =
    W.WAVE
        { W.waveHeader = header audio
        , W.waveSamples = [ toSamples audio ]
        }
        & W.putWAVEFile (T.unpack fn)
        & Cmd.fromIO


play :: Text -> Cmd
play 
    = Cmd.fromIO
    . SP.callCommand
    . T.unpack
    . T.append "play "


header :: Audio -> W.WAVEHeader
header audio =
    W.WAVEHeader
        { W.waveNumChannels = 1
        , W.waveFrameRate = 44100
        , W.waveBitsPerSample = 32
        , W.waveFrames = Just $ Audio.length audio 
        }
