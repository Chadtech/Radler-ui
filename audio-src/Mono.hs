{-# LANGUAGE OverloadedStrings #-}


module Mono
    ( Mono
    , Mono.map
    , append
    , applyFrom
    , applyUntil
    , compress
    , Mono.concat
    , convolve
    , declip
    , declip__testable
    , delay
    , deltaConvolve
    , equalizeLengths
    , empty
    , fromVector
    , fromList
    , fromSample
    , indexedMap
    , invert
    , Mono.length
    , maxInt32Sample
    , mix
    , mixMany
    , saw
    , saw__testable
    , setVolume
    , silence
    , Mono.sin
    , singleton
    , sinByWaveCount
    , Mono.splitAt
    , Mono.subtract
    , tiltedSin
    , toList
    , toSamples
    , toVector
    , trimEnd
    ) where


import Flow
import Prelude.Extra

import qualified Control.Monad as CM
import qualified Data.Fixed as Fixed
import Data.Int (Int32)
import qualified Data.List as List
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector as V
import qualified Data.Tuple.Extra as Tuple
import Freq (Freq(Freq))
import Duration (Duration(Duration))
import qualified Duration
import Volume (Volume(Volume))
import qualified Volume
import Timing (Timing)
import qualified Timing


-- TYPES --


data Mono
    = Mono (Vector Float)
    deriving (Eq)


instance Show Mono where
    show (Mono mono) =
        "Mono " ++ show mono


-- HELPERS --


trimEnd :: Mono -> Mono
trimEnd (Mono vector) =
    let
        isSampleZero :: Int -> Float -> Maybe Int -> Maybe Int
        isSampleZero index sample maybeLastNonZeroIndex =
            if 
                maybeLastNonZeroIndex == Nothing 
                    && sample /= 0
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
                |> Mono

        Nothing ->
            Mono vector


map :: (Float -> Float) -> Mono -> Mono
map f (Mono mono) =
    Mono <| Vector.map f mono


indexedMap :: (Int -> Float -> Float) -> Mono -> Mono
indexedMap f (Mono mono) =
    Mono <| Vector.imap f mono


applyUntil :: Int -> (Mono -> Mono) -> Mono -> Mono
applyUntil index f mono =
    let
        (beginning, end) =
            Mono.splitAt index mono
    in
    append (f beginning) end


applyFrom :: Int -> (Mono -> Mono) -> Mono -> Mono
applyFrom index f mono =
    let
        (beginning, end) =
            Mono.splitAt index mono
    in
    append beginning (f end)


toVector :: Mono -> Vector Float
toVector (Mono vector) =
    vector


fromVector :: Vector Float -> Mono
fromVector =
    Mono


fromList :: List Float -> Mono
fromList =
    Mono <. Vector.fromList


toList :: Mono -> List Float
toList (Mono mono) =
    Vector.toList mono


singleton :: Float -> Mono
singleton level =
    fromList [ level ]
    

empty :: Mono
empty =
    Mono Vector.empty


silence :: Duration -> Mono
silence duration =
    fromSample duration 0


fromSample :: Duration -> Float -> Mono
fromSample (Duration duration) float =
    Mono <| Vector.replicate duration float


tiltedSin :: Int -> Freq -> Duration -> Mono
tiltedSin degree (Freq freq) duration =
    let   
        tiltedSinForDegree :: Int -> Mono
        tiltedSinForDegree thisDegree =
            let
                thisDegreeFl :: Float
                thisDegreeFl =
                    toFloat thisDegree

                divideByDegree :: Float -> Float
                divideByDegree sample =
                    sample / thisDegreeFl

                generateThisDegree :: Float -> Float
                generateThisDegree sample =
                    (binomialCoefficient 
                        (2 * degree) 
                        (degree - thisDegree)
                        / binomialCoefficient 
                            (2 * degree) 
                            degree
                    ) * sample
            in
            sinInternal 
                0 
                (Freq (freq * thisDegreeFl)) 
                duration
                |> Vector.map divideByDegree
                |> Vector.map generateThisDegree
                |> Mono
    in
    List.map tiltedSinForDegree [1..degree]
        |> mixMany
        

binomialCoefficient :: Int -> Int -> Float
binomialCoefficient n k =
    let
        productFl :: Int -> Float
        productFl int =
            toFloat <| product [1..int]
    in
    productFl n / (productFl k * productFl (n - k))


sinByWaveCount :: Float -> Freq -> Int -> Mono
sinByWaveCount phase freq count =
    Mono.sin 
        phase
        freq 
        (Duration.fromFreqAndWaveCount freq count)


sin :: Float -> Freq -> Duration -> Mono
sin phase freq duration =
    sinInternal phase freq duration 
        |> Mono
    

sinInternal :: Float -> Freq -> Duration -> Vector Float
sinInternal phase (Freq freq) (Duration duration) =
    let
        sinAtSample :: Int -> Float
        sinAtSample index =
            Prelude.sin 
                ((2 * pi * (freq / 44100) * toFloat index ) + (pi * phase)) 
    in
    Vector.generate 
        duration 
        sinAtSample


saw :: Freq -> Duration -> Mono
saw =
    saw__testable 44100


saw__testable :: Int -> Freq -> Duration -> Mono
saw__testable samplesPerSecond (Freq freq) (Duration duration) =
    let
        freqInSamples :: Float
        freqInSamples =
            toFloat samplesPerSecond / freq


        sawAtSample :: Int -> Float
        sawAtSample index =
            let
                j :: Float
                j = 
                    toFloat index / freqInSamples
            in
            2 * (j - (toFloat <| floor (0.5 + j)))

    in
    Vector.generate duration sawAtSample
        |> Mono




delay :: Duration -> Mono -> Mono
delay delayAmount mono =
    append (silence delayAmount) mono


splitAt :: Int -> Mono -> (Mono, Mono)
splitAt index (Mono mono) =
    Vector.splitAt index mono
        |> Tuple.both Mono


mixMany :: List Mono -> Mono
mixMany !monos =
    case monos of
        first : second : rest ->
            mixMany (mix first second : rest)

        first : [] ->
            first

        [] ->
            empty


mix :: Mono -> Mono -> Mono
mix !mono0 !mono1 =
    let
        ( equalizedMono0, equalizedMono1 ) =
            equalizeLengths mono0 mono1
    in
    Mono.zip equalizedMono0 equalizedMono1

            
zip :: Mono -> Mono -> Mono
zip (Mono vector0) (Mono vector1) =
    Mono <| Vector.zipWith (+) vector0 vector1

        
setVolume :: Volume -> Mono -> Mono
setVolume (Volume newRelativeVolume) (Mono vector) =
    Mono <| Vector.map ((*) newRelativeVolume) vector


compress :: Int -> Mono -> Mono
compress times (Mono vector) =
    let
        compressSample :: Int -> Float -> Float
        compressSample remainingTimes s =
            if remainingTimes == 0 then
                s

            else
                compressSample
                    (remainingTimes - 1)
                    (s + (s * (1 - abs s)))
    in
    Mono <| Vector.map (compressSample times) vector



declip :: Mono -> Mono
declip =
    declip__testable 30


declip__testable :: Int -> Mono -> Mono
declip__testable declipLength (Mono mono)=
    let
        divideIndexBy :: Int -> Float
        divideIndexBy i =
            (fromIntegral i) 
                / (fromIntegral declipLength)


        declipIn :: Vector (Int, Float)
        declipIn =
            Vector.generate 
                declipLength 
                divideIndexBy
                |> Vector.indexed


        declipOut :: Vector (Int, Float)
        declipOut =
            Vector.generate 
                declipLength 
                ((-) 1 .> divideIndexBy)
                |> Vector.indexed
                |> Vector.map 
                    (Tuple.first ((+) (Vector.length mono - declipLength)))
    in
    Vector.accumulate
        (*)
        mono
        (Vector.concat [ declipIn, declipOut ])
        |> Mono


lopass :: Volume -> Int -> Mono -> Mono
lopass mixLevel samplesToSpan mono =
    mono
        |> lopassHelper samplesToSpan
        |> setVolume mixLevel
        |> mix (setVolume (Volume.invert mixLevel) mono)


lopassHelper :: Int -> Mono -> Mono
lopassHelper samplesToSpan mono =
    mono
        |> append (silence <| Duration samplesToSpan)
        |> averageSamples samplesToSpan


averageSamples :: Int -> Mono -> Mono
averageSamples samplesToSpan (Mono vector) =
    vector
        |> Vector.imap (averageSample samplesToSpan vector)
        |> Vector.drop samplesToSpan
        |> Mono


averageSample :: Int -> Vector Float -> Int -> Float -> Float
averageSample samplesToSpan samples index sample =
    samples
        |> Vector.slice index samplesToSpan
        |> Vector.foldl (+) 0
        |> divideBy samplesToSpan


divideBy :: Int -> Float -> Float
divideBy samplesToSpan sum =
    sum / toFloat samplesToSpan


equalizeLengths :: Mono -> Mono -> (Mono, Mono)
equalizeLengths mono0 mono1 =
    let
        mono0Length :: Int
        mono0Length =
            Mono.length mono0


        mono1Length :: Int
        mono1Length =
            Mono.length mono1

    in
    if mono0Length > mono1Length then
        ( mono0
        , appendSilence 
            (Duration (mono0Length - mono1Length))
            mono1
        )

    else
        ( appendSilence 
            (Duration (mono1Length - mono0Length))
            mono0
        , mono1
        )


appendSilence :: Duration -> Mono -> Mono
appendSilence duration mono =
    append mono <| silence duration


append :: Mono -> Mono -> Mono
append (Mono vector0) (Mono vector1) =
    Mono <| Vector.concat [ vector0, vector1]


concat :: List Mono -> Mono
concat monos =
    case monos of
        first : [] ->
            first

        first : rest ->
            append first <| Mono.concat rest

        [] ->
            empty


length :: Mono -> Int
length (Mono vector) =
    Vector.length vector


toSamples :: Mono -> List Int32
toSamples (Mono vector) =
    vector
        |> Vector.toList
        |> List.map 
            (toInt32 <. (*) maxInt32Sample)


maxInt32Sample :: Float
maxInt32Sample =
    2147483647


toInt32 :: Float -> Int32
toInt32 =
    truncate


convolve :: Mono -> Mono -> Mono
convolve (Mono seed) (Mono sound) =
    let
        indexedSeed :: Vector (Int, Float)
        indexedSeed =
            Vector.indexed seed


        timesThisSample :: (Int, Float) -> (Int, Float) -> (Int, Float)
        timesThisSample (thisSampleIndex, thisSample) (seedIndex, thisSeedSample) =
            ( thisSampleIndex + seedIndex
            , thisSample * thisSeedSample
            )


        convolveHelper :: List (Int, Float) -> Vector Float -> Vector Float
        convolveHelper sound output =
            case sound of
                sample : rest ->
                    indexedSeed
                        |> Vector.map (timesThisSample sample)
                        |> Vector.accumulate (+) output
                        |> convolveHelper rest
        
                [] ->
                    output

    in
    Vector.replicate 
        (Vector.length seed + Vector.length sound)
        0
        |> convolveHelper 
            (Vector.toList <| Vector.indexed sound) 
        |> Mono


deltaConvolve :: Mono -> Mono -> Mono
deltaConvolve seed sound =
    mix
        sound
        (convolve seed (delta sound))
        |> setVolume (Volume 0.5)


delta :: Mono -> Mono
delta (Mono mono) =
    let
        sampleDifference :: (Float, Float) -> Float
        sampleDifference (first, second) =
            (second - first) / 2
    in
    Vector.zip
        (Vector.cons 0 mono)
        mono
        |> Vector.map sampleDifference
        |> Mono


subtract :: Mono -> Mono -> Mono
subtract mono =
    mix (invert mono)


invert :: Mono -> Mono
invert (Mono mono) =
    Mono <| Vector.map ((*) (-1)) mono
        