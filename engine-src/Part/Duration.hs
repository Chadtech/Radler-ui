{-# LANGUAGE OverloadedStrings #-}


module Part.Duration
    ( Part.Duration.read
    , Duration(..)
    , Error
    , fromFreqAndWaveCount
    , throw
    ) where


import Flow
import Prelude.Extra

import Config (Config)
import qualified Config
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import Freq (Freq)
import qualified Freq


-- TYPES --


newtype Duration 
    = Duration Int
    deriving (Eq)


-- HELPERS --


read :: Config -> Text -> Either Error Duration
read config txt =
    case TR.hexadecimal txt of
        Right (v, _) ->
            v * Config.beatLength config
                |> Duration
                |> Right

        Left err ->
            err
                |> T.pack
                |> TextNotHexadecimal
                |> Left
                

fromFreqAndWaveCount :: Freq -> Int -> Duration
fromFreqAndWaveCount freq count =
    (44100 / Freq.toFloat freq)
        |> (*) (toFloat count)
        |> floor
        |> Duration


-- ERROR --


data Error 
    = TextNotHexadecimal Text


throw :: Error -> Text
throw error =
    case error of
        TextNotHexadecimal txt ->
            [ "Duration is not hexadecimal : "
            , txt
            ]
                |> T.concat
