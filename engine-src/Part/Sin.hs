{-# LANGUAGE OverloadedStrings #-}


module Part.Sin
    ( toMono
    ) where


import Flow
import Prelude.Extra
        
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import qualified Part.Osc as Osc


toMono :: Osc.Note -> Mono
toMono note = 
    Mono.sin 
        (Osc.freq note)
        (Osc.duration note)
        |> Mono.setVolume (Osc.volume note)
        |> Mono.declip

