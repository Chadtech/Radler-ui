{-# LANGUAGE OverloadedStrings #-}


module Part.Volume
    ( Part.Volume.read
    , Error
    , throw
    ) where


import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (Parser)
import qualified Parse
import Result (Result(Ok, Err))
import qualified Result 


read :: Text -> Result Error Float
read txt =
    Ok 1



-- ERROR --


data Error 
    = TextNotHexidecimal Text


throw :: Error -> Text
throw error =
    case error of
        TextNotHexidecimal txt ->
            [ "Text is not hexidecimal : "
            , txt
            ]
                & T.concat
