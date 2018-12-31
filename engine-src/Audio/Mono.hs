{-# LANGUAGE OverloadedStrings #-}


module Audio.Mono
    ( Mono
    , compress
    , declip
    , empty
    , fromTimeline
    , fromVector
    , Audio.Mono.length
    , mix
    , mixMany
    , setVolume
    , silence
    , toSamples
    , toVector
    , Audio.Mono.sin
    ) where


import qualified Control.Monad as CM
import Data.Function ((&))
import Data.Int (Int32)
import qualified Data.List as List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude.Extra (List, mapFirst, toFloat)


data Mono
    = Mono (Vector Float)


toVector :: Mono -> Vector Float
toVector (Mono vector) =
    vector


fromVector :: Vector Float -> Mono
fromVector =
    Mono
    

empty :: Mono
empty =
    Mono Vector.empty


silence :: Int -> Mono
silence duration =
    Mono $ Vector.replicate duration 0


sin :: Float -> Int -> Mono
sin freq duration =
    Vector.generate 
        duration 
        (sinAtSample freq)
        & Mono


sinAtSample :: Float -> Int -> Float
sinAtSample freq index =
    Prelude.sin (2 * pi * (freq / 44100) * toFloat index) 


mixMany :: List Mono -> Mono
mixMany monos =
    case monos of
        first : second : rest ->
            mix 
                (mix first second)
                (mixMany rest)

        first : [] ->
            first

        [] ->
            empty


mix :: Mono -> Mono -> Mono
mix mono0 mono1 =
    let
        ( equalizedMono0, equalizedMono1 ) =
            equalizeLengths mono0 mono1
    in
    Audio.Mono.zip equalizedMono0 equalizedMono1

            
zip :: Mono -> Mono -> Mono
zip (Mono vector0) (Mono vector1) =
    Mono $ Vector.zipWith (+) vector0 vector1

        
setVolume :: Float -> Mono -> Mono
setVolume newRelativeVolume (Mono vector) =
    Mono $ Vector.map ((*) newRelativeVolume) vector


compress :: Mono -> Mono
compress (Mono vector) =
    Mono $ Vector.map compressSample vector


compressSample :: Float -> Float
compressSample s =
    s + (s * (1 - s))


fromTimeline :: Vector (Int, Mono) -> Mono
fromTimeline timeline =
    if Vector.length timeline == 0 then
        empty

    else
        Vector.accumulate 
            (+)
            (timelineBasis $ Vector.last timeline)
            (CM.join $ Vector.map timelineSamples timeline)
            & Mono


declip :: Mono -> Mono
declip (Mono vector) =
    Vector.accumulate
        (*)
        vector
        (declipVector (Vector.length vector))
        & Mono


declipVector :: Int -> Vector (Int, Float)
declipVector length =
    [ declipIn
    , declipOut length
    ]
        & Vector.concat


declipIn :: Vector (Int, Float)
declipIn =
    Vector.generate 
        declipLength 
        divideIndexBy
        & Vector.indexed


declipOut :: Int -> Vector (Int, Float)
declipOut length =
    Vector.generate 
        declipLength 
        ((-) 1 . divideIndexBy)
        & Vector.indexed
        & Vector.map (mapFirst ((+) (length - declipLength)))


declipLength :: Int
declipLength =
    120


divideIndexBy :: Int -> Float
divideIndexBy i =
    ((fromIntegral i) / (fromIntegral declipLength)) ^ 2


timelineBasis :: (Int, Mono) -> Vector Float 
timelineBasis (lastStartingPoint, lastMono) =
    lastStartingPoint + Audio.Mono.length lastMono
        & silence
        & toVector


timelineSamples :: (Int, Mono) -> Vector (Int, Float)
timelineSamples (beginningIndex, Mono vector) =
    vector
        & Vector.indexed
        & Vector.map (mapFirst ((+) beginningIndex))


lopass :: Float -> Int -> Mono -> Mono
lopass mixLevel samplesToSpan mono =
    mono
        & lopassHelper samplesToSpan
        & setVolume mixLevel
        & mix (setVolume (1 - mixLevel) mono)


lopassHelper :: Int -> Mono -> Mono
lopassHelper samplesToSpan mono =
    mono
        & append (silence samplesToSpan)
        & averageSamples samplesToSpan


averageSamples :: Int -> Mono -> Mono
averageSamples samplesToSpan (Mono vector) =
    vector
        & Vector.imap (averageSample samplesToSpan vector)
        & Vector.drop samplesToSpan
        & Mono


averageSample :: Int -> Vector Float -> Int -> Float -> Float
averageSample samplesToSpan samples index sample =
    samples
        & Vector.slice index samplesToSpan
        & Vector.foldl (+) 0
        & divideBy samplesToSpan


divideBy :: Int -> Float -> Float
divideBy samplesToSpan sum =
    sum / toFloat samplesToSpan


equalizeLengths :: Mono -> Mono -> (Mono, Mono)
equalizeLengths mono0 mono1 =
    let
        mono0Length :: Int
        mono0Length =
            Audio.Mono.length mono0

        mono1Length :: Int
        mono1Length =
            Audio.Mono.length mono1

    in
    if mono0Length > mono1Length then
        ( mono0
        , appendSilence 
            (mono0Length - mono1Length) 
            mono1
        )

    else
        ( appendSilence 
            (mono1Length - mono0Length) 
            mono0
        , mono1
        )


appendSilence :: Int -> Mono -> Mono
appendSilence duration mono =
    append mono (silence duration)


append :: Mono -> Mono -> Mono
append (Mono vector0) (Mono vector1) =
    Mono $ Vector.concat [ vector0, vector1]


length :: Mono -> Int
length (Mono vector) =
    Vector.length vector


toSamples :: Mono -> List Int32
toSamples (Mono vector) =
    vector
        & Vector.toList
        & List.map 
            (toInt32 . (*) 2147483647)


toInt32 :: Float -> Int32
toInt32 =
    round

