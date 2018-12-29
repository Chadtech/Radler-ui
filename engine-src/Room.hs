{-# LANGUAGE OverloadedStrings #-}


module Room
    ( Room
    , Room.read
    , Error
    , throw
    ) where


import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (Parser)
import qualified Parse
import Position (Position)
import qualified Position
import Result (Result(Ok, Err))
import qualified Result


-- TYPES --
    

data Room
    = Room
        { listenerPosition :: Position
        } 
        deriving (Eq)



-- HELPERS --


read :: Text -> Result Error Room
read roomText =
    Room
        & Ok
        & applyListenerPosition roomText


applyListenerPosition :: Text -> Parser Error Position b
applyListenerPosition roomText ctorResult =
    case Position.read roomText of
        Ok position ->
            Parse.construct position ctorResult

        Err error ->
            Err $ PositionError error


-- ERROR --


data Error
    = PositionError Position.Error


throw :: Error -> Text
throw error =
    case error of
        PositionError error ->
            Position.throw error

        