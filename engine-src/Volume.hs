{-# LANGUAGE OverloadedStrings #-}


module Volume
    ( Volume(..)
    , Volume.read
    , Error
    , Volume.map
    , applyTo
    , throw
    , invert
    , multiply
    , toFloat
    ) where


import Flow

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as TR


-- TYPES --


newtype Volume
    = Volume Float
    deriving (Eq)


instance Show Volume where
    show (Volume volume) =
        [ "Volume "
        , T.pack <| show volume
        ]
            |> T.concat
            |> T.unpack


-- HELPERS --


toFloat :: Volume -> Float
toFloat (Volume float) =
    float

    
applyTo :: Volume -> Float -> Float
applyTo (Volume vol) fl =
    fl * vol


invert :: Volume -> Volume
invert (Volume fl) =
    Volume (1 - fl)


map :: (Float -> Float) -> Volume -> Volume
map f (Volume fl) =
    Volume (f fl)


multiply :: Volume -> Volume -> Volume
multiply (Volume v0) (Volume v1) =
    Volume (v0 * v1)


read :: Text -> Either Error Volume
read txt =
    case TR.hexadecimal txt of
        Right (v, _) ->
            fromIntegral v / 255
                |> Volume
                |> Right

        Left err ->
            err
                |> T.pack
                |> TextNotHexadecimal
                |> Left
                

-- ERROR --


data Error 
    = TextNotHexadecimal Text
    deriving (Eq)


throw :: Error -> Text
throw error =
    case error of
        TextNotHexadecimal txt ->
            [ "Volume is not hexadecimal : "
            , txt
            ]
                |> T.concat
