{-# LANGUAGE OverloadedStrings #-}


module Part.Sin
  ( Model
  , makeFlags
  , toMono
  )
where


import           Flow
import           Prelude.Extra

import           Mono                           ( Mono )
import qualified Mono
import qualified Contour
import           Data.Text.Lazy                 ( Text )
import           Parse                          ( parse )
import qualified Parse
import qualified Part.Osc                      as Osc


-- TYPES --


data Model
    = Model
    deriving (Eq)


-- HELPERS --


makeFlags :: Parse.Fields Text -> Osc.Flags Model
makeFlags fields = Osc.Flags toMono fields Model


toMono :: Model -> Osc.Note -> Mono
toMono _ note =
  Mono.sin 0 (Osc.freq note) (Osc.duration note)
    |> Contour.apply (Osc.contour note)
    |> Mono.setVolume (Osc.volume note)
    |> Mono.declip

