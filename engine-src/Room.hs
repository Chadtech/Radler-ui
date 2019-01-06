{-# LANGUAGE OverloadedStrings #-}


module Room
    ( Room
    , Room.read
    , listenerPosition
    , size
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
import qualified Size
import Size (Size)


-- TYPES --
    

--        |------------|
--        |            |
--        |            |
--        |            |
--        |            |
--       ^|            |
--       ||            |
--  length|------------|
--        width ->



data Room
    = Room
        { listenerPosition :: Position
        , size :: Size
        } 
        deriving (Eq)



-- HELPERS --


read :: Text -> Result Error (Maybe Room)
read roomText =
    if roomText == "no-room" then
        Ok Nothing

    else
        let
            fieldsResult :: Result Text (Parse.Fields Float)
            fieldsResult =
                Parse.fields Parse.float roomText
        in
        case fieldsResult of
            Ok fields ->
                Room
                    & Ok
                    & parse (Position.read fields) PositionError
                    & parse (Size.read fields) SizeError
                    & Result.map Just

            Err err ->
                Err $ ParsingFailed err


-- ERROR --


data Error
    = PositionError Position.Error
    | SizeError Size.Error
    | ParsingFailed Text


throw :: Error -> Text
throw error =
    case error of
        PositionError error ->
            Position.throw error

        SizeError error ->
            Size.throw error

        ParsingFailed error ->
            T.append "Parsing failed - > " error

        