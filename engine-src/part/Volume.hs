{-# LANGUAGE OverloadedStrings #-}


module Part.Volume
    ( Part.Volume.read
    , Error
    , throw
    ) where


import Flow

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR


read :: Text -> Either Error Float
read txt =
    case TR.hexadecimal txt of
        Right (v, _) ->
            Right (fromIntegral v / 255)

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
            [ "Volume is not hexadecimal : "
            , txt
            ]
                |> T.concat
