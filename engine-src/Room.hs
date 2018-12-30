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
import Parse (parse)
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


read :: Text -> Result Error (Maybe Room)
read roomText =
    if roomText == "no-room" then
        Ok Nothing

    else
        Room
            & Ok
            & parse (Position.read roomText) PositionError
            & Result.map Just

-- ERROR --


data Error
    = PositionError Position.Error


throw :: Error -> Text
throw error =
    case error of
        PositionError error ->
            Position.throw error

        