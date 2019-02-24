{-# LANGUAGE OverloadedStrings #-}


module Audio.Mono
    ( Mono
    , Audio.Mono.map
    , compress
    , convolve_
    , declip
    , tiltedSin
    , empty
    , fadeIn
    , fadeOut
    , singleton
    , fromVector
    , Audio.Mono.length
    , mix
    , mixMany
    , setVolume
    , silence
    , toSamples
    , toVector
    , delay
    , Audio.Mono.subtract
    , Audio.Mono.sin
    , saw
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
import Part.Duration (Duration(Duration))
import Part.Volume (Volume(Volume))
import qualified Part.Volume as Volume


-- TYPES --


data Mono
    = Mono (Vector Float)


instance Show Mono where
    show (Mono mono) =
        show mono


-- HELPERS --


map :: (Float -> Float) -> Mono -> Mono
map f (Mono mono) =
    Mono <| Vector.map f mono

toVector :: Mono -> Vector Float
toVector (Mono vector) =
    vector


fromVector :: Vector Float -> Mono
fromVector =
    Mono


singleton :: Mono
singleton =
    Mono <| Vector.fromList $ [ 1 ]
    

empty :: Mono
empty =
    Mono Vector.empty


silence :: Int -> Mono
silence duration =
    Mono $ Vector.replicate duration 0


tiltedSin :: Int -> Freq -> Duration -> Mono
tiltedSin degree (Freq freq) duration =
    let
        tiltedSinForDegree :: Int -> Mono
        tiltedSinForDegree thisDegree =
            sinInternal 
                0 
                (Freq (freq * thisDegree)) 
                duration
                |> Vector.map (\s -> s / thisDegree)
                |> Vector.map (\s ->
                    (toFloat (binomialCoefficient (2 * degree) (degree - thisDegree))
                        / (toFloat (binomialCoefficient (2 * degree) degree))
                    ) * s
                )        
                |> Mono
    in
    List.map tiltedSinForDegree [1..degree]
        |> mixMany
        

binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k =
    product [1..n] / (product [1..k] * product [1..(n - k)])


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
saw (Freq freq) (Duration duration) =
    Vector.generate
        duration
        (sawAtSample freq)
        |> Mono


sawAtSample :: Float -> Int -> Float
sawAtSample freq index =
    let
        j :: Float
        j = 
            toFloat index / (44100 / freq)
    in
    2 * (j - (toFloat <| floor (0.5 + j)))


delay :: Int -> Mono -> Mono
delay delayAmount mono =
    append (silence delayAmount) mono


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
    Audio.Mono.zip equalizedMono0 equalizedMono1

            
zip :: Mono -> Mono -> Mono
zip (Mono vector0) (Mono vector1) =
    Mono <| Vector.zipWith (+) vector0 vector1

        
setVolume :: Volume -> Mono -> Mono
setVolume (Volume newRelativeVolume) (Mono vector) =
    Mono <| Vector.map ((*) newRelativeVolume) vector


compress :: Mono -> Mono
compress (Mono vector) =
    Mono <| Vector.map compressSample vector


compressSample :: Float -> Float
compressSample s =
    s + (s * (1 - s))


declip :: Mono -> Mono
declip (Mono vector) =
    Vector.accumulate
        (*)
        vector
        (declipVector (Vector.length vector))
        |> Mono


declipVector :: Int -> Vector (Int, Float)
declipVector length =
    [ declipIn
    , declipOut length
    ]
        |> Vector.concat


declipIn :: Vector (Int, Float)
declipIn =
    Vector.generate 
        declipLength 
        divideIndexBy
        |> Vector.indexed


declipOut :: Int -> Vector (Int, Float)
declipOut length =
    Vector.generate 
        declipLength 
        ((-) 1 . divideIndexBy)
        |> Vector.indexed
        |> Vector.map (Tuple.first ((+) (length - declipLength)))


declipLength :: Int
declipLength =
    120


fadeIn :: Mono -> Mono
fadeIn (Mono mono) =
    let
        lengthOfMono :: Float
        lengthOfMono =
            mono
                |> Vector.length
                |> toFloat

        divideByLength :: Int -> Float -> Float
        divideByLength index sample =
            sample * (toFloat index / lengthOfMono)
    in
    Vector.imap 
        divideByLength
        mono 
        |> Mono


fadeOut :: Mono -> Mono
fadeOut (Mono mono) =
    let
        lengthOfMono :: Float
        lengthOfMono =
            mono
                |> Vector.length
                |> toFloat

        divideByLength :: Int -> Float -> Float
        divideByLength index sample =
            sample * (1 - (toFloat index / lengthOfMono))
    in
    Vector.imap 
        divideByLength
        mono 
        |> Mono


divideIndexBy :: Int -> Float
divideIndexBy i =
    ((fromIntegral i) / (fromIntegral declipLength)) ^ 2


lopass :: Volume -> Int -> Mono -> Mono
lopass mixLevel samplesToSpan mono =
    mono
        |> lopassHelper samplesToSpan
        |> setVolume mixLevel
        |> mix (setVolume (Volume.invert mixLevel) mono)


lopassHelper :: Int -> Mono -> Mono
lopassHelper samplesToSpan mono =
    mono
        |> append (silence samplesToSpan)
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
    Mono <| Vector.concat [ vector0, vector1]


length :: Mono -> Int
length (Mono vector) =
    Vector.length vector


toSamples :: Mono -> List Int32
toSamples (Mono vector) =
    vector
        |> Vector.toList
        |> List.map 
            (toInt32 . (*) 2147483647)


toInt32 :: Float -> Int32
toInt32 =
    round


convolve_ :: Mono -> Mono -> Mono
convolve_ (Mono seed) (Mono sound) =
    convolveHelper 
        (Vector.indexed seed) 
        (Vector.toList $ Vector.indexed sound) 
        (newConvolvedBase seed sound)
        |> Mono


convolveHelper :: Vector (Int, Float) -> List (Int, Float) -> Vector Float -> Vector Float
convolveHelper seed sound output =
    case sound of
        (index, sample) : rest ->
            let
                seedAtVolume =
                    multiplySeed sample seed
            in
            seedAtVolume
                |> addToIndices index
                |> Vector.accumulate (+) output
                |> convolveHelper seed rest

        [] ->
            output


newConvolvedBase :: Vector Float -> Vector Float -> Vector Float
newConvolvedBase seed audio =
    Vector.replicate 
        (Vector.length seed + Vector.length audio)
        0

        
addToIndices :: Int -> Vector (Int, Float) -> Vector (Int, Float) 
addToIndices index = 
    Vector.map 
        (Tuple.first ((+) index))


multiplySeed :: Float -> Vector (Int, Float) -> Vector (Int, Float)
multiplySeed audioSample = 
    Vector.map 
        (Tuple.second ((*) audioSample))


subtract :: Mono -> Mono -> Mono
subtract (Mono monoVector) =
    mix (Mono <| Vector.map ((*) (-1)) monoVector)
        
