{-# LANGUAGE OverloadedStrings #-}


module Audio
    ( Audio
    , fromMono
    , fromStereo
    , mixMany
    , mix 
    , normalizeVolumes
    , play
    , setVolume
    , Audio.subtract
    , write
    ) where


import Flow
import Prelude.Extra

import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Audio.Stereo (Stereo)
import qualified Audio.Stereo as Stereo
import Cmd (Cmd)
import qualified Cmd
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.WAVE (WAVE)
import qualified Data.WAVE as W
import qualified System.Process as SP
import Part.Volume (Volume(..))


-- TYPES --


data Audio
    = Mono Mono
    | Stereo Stereo


instance Show Audio where
    show audio =
        case audio of
            Mono mono ->
                show mono

            Stereo stereo ->
                show stereo


-- HELPERS --

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


write :: Text -> Audio -> Cmd
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
        |> Cmd.fromIO


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


play :: Text -> Cmd
play fileName =
    fileName
        |> T.append "play "
        |> T.unpack
        |> SP.callCommand
        |> Cmd.fromIO
    