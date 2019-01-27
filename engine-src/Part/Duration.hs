{-# LANGUAGE OverloadedStrings #-}


module Part.Duration
    ( Part.Duration.read
    , Error
    , throw
    ) where


import Flow

import Config (Config)
import qualified Config
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR


read :: Config -> Text -> Either Error Int
read config txt =
    case TR.hexadecimal txt of
        Right (v, _) ->
            Right <| v * Config.beatLength config

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
