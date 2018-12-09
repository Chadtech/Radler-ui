{-# LANGUAGE OverloadedStrings #-}


module Part.Duration
    ( Part.Duration.read
    , Error
    , throw
    ) where


import Config (Config)
import qualified Config
import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import Parse (Parser)
import qualified Parse
import Result (Result(Ok, Err))
import qualified Result 


read :: Config -> Text -> Result Error Int
read config txt =
    case TR.hexadecimal txt of
        Right (v, _) ->
            Ok (v * Config.beatLength config)

        Left err ->
            err
                & T.pack
                & TextNotHexadecimal
                & Err
                

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
                & T.concat
