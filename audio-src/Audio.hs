{-# LANGUAGE OverloadedStrings #-}


module Audio
    ( Audio
    , Audio.concat
    , Audio.read
    , append
    , empty
    , fromMono
    , fromList
    , trimEnd
    , fromStereo
    , mixMany
    , mix 
    , normalizeVolumes
    , play
    , setVolume
    , Audio.subtract
    , write
    , ReadError
    ) where


import Flow
import Prelude.Extra

import Mono (Mono)
import qualified Mono
import Stereo (Stereo)
import qualified Stereo

import qualified Control.Monad as CM
import qualified Data.Either.Extra as Either
import Data.Int (Int64, Int32)
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.WAVE (WAVE)
import qualified Data.WAVE as W
import qualified System.Process as SP
import Volume (Volume(Volume))

-- TYPES --


data Audio
    = Mono Mono
    | Stereo Stereo
    deriving (Eq)


instance Show Audio where
    show audio =
        case audio of
            Mono mono ->
                show mono

            Stereo stereo ->
                show stereo


-- HELPERS --


append :: Audio -> Audio -> Audio
append audio0 audio1 =
    case (audio0, audio1) of
        (Stereo stereo0, Stereo stereo1) ->
            Stereo <| Stereo.append stereo0 stereo1

        (Mono mono, Stereo stereo) ->
            Stereo <| Stereo.append (Stereo.fromMono mono) stereo

        (Stereo stereo, Mono mono) ->
            Stereo <| Stereo.append stereo (Stereo.fromMono mono)

        (Mono mono0, Mono mono1) ->
            Mono <| Mono.append mono0 mono1


concat :: List Audio -> Audio
concat audios =
    case audios of
        first : [] ->
            first

        first : rest ->
            append first <| Audio.concat rest

        [] ->
            empty


trimEnd :: Audio -> Audio
trimEnd audio =
    case audio of
        Mono mono ->
            Mono.trimEnd mono
                |> Mono

        Stereo stereo ->
            Stereo.trimEnd stereo
                |> Stereo


fromList :: List Float -> Audio
fromList =
    Mono <. Mono.fromList


fromMono :: Mono -> Audio
fromMono =
    Mono


fromStereo :: Stereo -> Audio
fromStereo =
    Stereo


empty :: Audio
empty =
    Mono Mono.empty


normalizeVolumes :: List Audio -> List Audio
normalizeVolumes audios =
    List.map 
        (setVolume (Volume (1 / (toFloat (List.length audios)))))
        audios


setVolume :: Volume -> Audio -> Audio
setVolume newRelativeVolume audio =
    case audio of
        Mono mono ->
            Mono <| Mono.setVolume newRelativeVolume mono

        Stereo stereo ->
            Stereo <| Stereo.setVolume newRelativeVolume stereo


subtract :: Audio -> Audio -> Audio
subtract audio subtractFrom =
    case (audio, subtractFrom) of
        (Mono mono, Stereo stereo) ->
            Stereo.subtract (Stereo.fromMono mono) stereo
                |> Audio.fromStereo

        (Mono m0, Mono m1) ->
            Mono.subtract m0 m1
                |> Audio.fromMono

        (Stereo stereo, Mono mono) ->
            Mono.subtract 
                (Stereo.toMono stereo)
                mono
                |> Audio.fromMono

        (Stereo s0, Stereo s1) ->
            Stereo.subtract s0 s1
                |> Audio.fromStereo


mixMany :: List Audio -> Audio
mixMany !audios =
    case audios of
        Mono mono0 : Mono mono1 : rest ->
            mix
                (fromMono <| Mono.mix mono0 mono1)
                (mixMany rest)

        Stereo stereo : Mono mono : rest ->
            mix 
                (fromStereo <| Stereo.mix stereo (Stereo.fromMono mono))
                (mixMany rest)

        Mono mono : Stereo stereo : rest ->
            mix 
                (fromStereo <| Stereo.mix (Stereo.fromMono mono) stereo)
                (mixMany rest)

        Stereo stereo0 : Stereo stereo1 : rest ->
            mix
                (fromStereo <| Stereo.mix stereo0 stereo1)
                (mixMany rest)

        audio : [] ->
            audio

        [] ->
            empty


mix :: Audio -> Audio -> Audio
mix audio0 audio1 =
    case (audio0, audio1) of
        (Mono mono0, Mono mono1) ->
            Mono <| Mono.mix mono0 mono1

        (Stereo stereo, Mono mono) ->
            mono
                |> Stereo.fromMono
                |> Stereo.mix stereo
                |> Stereo

        (Mono mono, Stereo stereo) ->
            Stereo.mix 
                (Stereo.fromMono mono)
                stereo
                |> Stereo

        (Stereo stereo0, Stereo stereo1) ->
            Stereo <| Stereo.mix stereo0 stereo1


write :: Text -> Audio -> IO ()
write fn audio =
    W.WAVE
        { W.waveHeader = header audio
        , W.waveSamples = 
            case audio of
                Mono mono ->
                    [ Mono.toSamples mono ]

                Stereo stereo ->
                    let
                        (left, right) =
                            Stereo.toMonos stereo
                    in
                    [ mixLists
                        (Mono.toSamples left)
                        (Mono.toSamples right)
                    ]

        }
        |> W.putWAVEFile (T.unpack fn)



data ReadError
    = MoreThanTwoChannels
    | ImpossibleCaseOfMoreThanOneChannel


read :: Text -> IO Audio
read fn =
    let
        divideTo1 :: Float -> Float
        divideTo1 fl =
            fl / maxInt32Sample


        frameToMono :: List Int32 -> IO Float
        frameToMono frame =
            case frame of
                sample : [] ->
                    sample
                        |> fromInt32
                        |> toFloat
                        |> divideTo1
                        |> return

                _ ->
                    error "impossible case of more than one channel for single channel audio"

        toMono :: List (List Int32) -> IO Mono
        toMono samples =
            samples
                |> List.map frameToMono
                |> CM.sequence
                |> fmap Mono.fromList


        toAudio :: W.WAVE -> IO Audio
        toAudio wave =
            case W.waveNumChannels <| W.waveHeader wave of
                1 ->
                    Mono.read fn
                        |> mapIO Mono

                2 ->
                    return empty

                _ ->
                    error "more than two channels"

    in
    W.getWAVEFile (T.unpack fn)
        |> andThen toAudio


header :: Audio -> W.WAVEHeader
header audio =
    W.WAVEHeader
        { W.waveNumChannels = 
            case audio of
                Mono _ ->
                    1

                Stereo _ ->
                    2
        , W.waveFrameRate = 44100
        , W.waveBitsPerSample = 32
        , W.waveFrames = Just <| Audio.length audio
        }


length :: Audio -> Int
length audio =
    case audio of
        Mono mono ->
            Mono.length mono

        Stereo stereo ->
            Stereo.length stereo


play :: Text -> IO ()
play fileName =
    fileName
        |> T.append "play "
        |> T.unpack
        |> SP.callCommand
    