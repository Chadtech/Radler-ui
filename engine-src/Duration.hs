{-# LANGUAGE OverloadedStrings #-}


module Duration
    ( Duration.read
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


instance Show Duration where
    show (Duration duration) =
        [ "Duration "
        , T.pack <| show duration
        ]
            |> T.concat
            |> T.unpack


-- HELPERS --


read :: Int -> Text -> Either Error Duration
read resolution txt =
    case TR.hexadecimal txt of
        Right (v, _) ->
            v * resolution
--             v * Config.beatLength config
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
    deriving (Eq)


throw :: Error -> Text
throw error =
    case error of
        TextNotHexadecimal txt ->
            [ "Duration is not hexadecimal : "
            , txt
            ]
                |> T.concat
