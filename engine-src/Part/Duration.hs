{-# LANGUAGE OverloadedStrings #-}


module Part.Duration
    ( Part.Duration.read
    , Duration(..)
    , Error
    , throw
    ) where


import Flow

import Config (Config)
import qualified Config
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR


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
