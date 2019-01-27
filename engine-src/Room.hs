{-# LANGUAGE OverloadedStrings #-}


module Room
    ( Room
    , Room.read
    , listenerPosition
    , size
    , Error
    , throw
    ) where


import Flow

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (parse)
import qualified Parse
import Position (Position)
import qualified Position
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


read :: Text -> Either Error (Maybe Room)
read roomText =
    if roomText == "no-room" then
        Right Nothing

    else
        let
            fieldsResult :: Either Text (Parse.Fields Float)
            fieldsResult =
                Parse.fields Parse.float roomText
        in
        case fieldsResult of
            Right fields ->
                Room
                    |> Right
                    |> parse (Position.read fields) PositionError
                    |> parse (Size.read fields) SizeError
                    |> Either.mapRight Just

            Left err ->
                Left <| ParsingFailed err


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

        