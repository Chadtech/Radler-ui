{-# LANGUAGE OverloadedStrings #-}


module Part.DullSaw
    ( Model
    , makeFlags
    , toMono
    ) where


import Flow
import Prelude.Extra
    
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import qualified Contour
import Data.Text.Lazy (Text)
import Parse (parse)
import qualified Parse
import qualified Part.Osc as Osc


-- TYPES --


data Model
    = Model
    deriving (Eq)


-- HELPERS --


makeFlags :: Parse.Fields Text -> Osc.Flags Model
makeFlags fields =
    Osc.Flags toMono fields Model


toMono :: Model -> Osc.Note -> Mono
toMono _ note = 
    let
        freq =
            Osc.freq note

        duration =
            Osc.duration note
    in
    Mono.mix
        (Mono.saw freq duration)
        (Mono.sin (-0.5) freq duration)
        |> Contour.apply (Osc.contour note)
        |> Mono.setVolume (Osc.volume note)
        |> Mono.declip

