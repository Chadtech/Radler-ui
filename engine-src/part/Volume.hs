{-# LANGUAGE OverloadedStrings #-}


module Part.Volume
    ( Part.Volume.read
    , Error
    , throw
    ) where


import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR
import Parse (Parser)
import qualified Parse
import Result (Result(Ok, Err))
import qualified Result 


import Prelude.Extra (debugLog)

read :: Text -> Result Error Float
read txt =
    case TR.hexadecimal txt of
        Right (v, _) ->
            Ok (fromIntegral v / 255)

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
            [ "Volume is not hexadecimal : "
            , txt
            ]
                & T.concat
