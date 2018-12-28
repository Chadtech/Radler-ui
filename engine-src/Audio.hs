{-# LANGUAGE OverloadedStrings #-}


module Audio
    ( Audio
    , fromMono
    , fromStereo
    , mixMany
    , normalizeVolumes
    , play
    , setVolume
    , write
    ) where


import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Audio.Stereo (Stereo)
import qualified Audio.Stereo as Stereo
import Cmd (Cmd)
import qualified Cmd
import Data.Function ((&))
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.WAVE (WAVE)
import qualified Data.WAVE as W
import Prelude.Extra (List, toFloat)
import qualified System.Process as SP


data Audio
    = Mono Mono
    | Stereo Stereo


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
        (setVolume (1 / (toFloat (List.length audios))))
        audios


setVolume :: Float -> Audio -> Audio
setVolume newRelativeVolume audio =
    case audio of
        Mono mono ->
            Mono $ Mono.setVolume newRelativeVolume mono

        Stereo stereo ->
            Stereo $ Stereo.setVolume newRelativeVolume stereo


mixMany :: List Audio -> Audio
mixMany audios =
    case audios of
        Mono mono0 : Mono mono1 : rest ->
            mix
                (fromMono $ Mono.mix mono0 mono1)
                (mixMany rest)

        Stereo stereo : Mono mono : rest ->
            mix 
                (fromStereo $ Stereo.mix stereo (Stereo.fromMono mono))
                (mixMany rest)

        Mono mono : Stereo stereo : rest ->
            mix 
                (fromStereo $ Stereo.mix (Stereo.fromMono mono) stereo)
                (mixMany rest)

        audio : [] ->
            audio

        [] ->
            empty


mix :: Audio -> Audio -> Audio
mix audio0 audio1 =
    case (audio0, audio1) of
        (Mono mono0, Mono mono1) ->
            Mono $ Mono.mix mono0 mono1

        (Stereo stereo, Mono mono) ->
            mono
                & Stereo.fromMono
                & Stereo.mix stereo
                & Stereo

        (Mono mono, Stereo stereo) ->
            Stereo.mix 
                (Stereo.fromMono mono)
                stereo
                & Stereo

        (Stereo stereo0, Stereo stereo1) ->
            Stereo $ Stereo.mix stereo0 stereo1


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
                    [ Mono.toSamples left
                    , Mono.toSamples right
                    ]

        }
        & W.putWAVEFile (T.unpack fn)
        & Cmd.fromIO


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
        , W.waveFrames = Just $ Audio.length audio
        }


length :: Audio -> Int
length audio =
    case audio of
        Mono mono ->
            Mono.length mono

        Stereo stereo ->
            Stereo.length stereo


play :: Text -> Cmd
play 
    = Cmd.fromIO
    . SP.callCommand
    . T.unpack
    . T.append "play "
    