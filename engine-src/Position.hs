{-# LANGUAGE OverloadedStrings #-}


module Position
    ( Position
    , x
    , y
    , Position.read
    , Error
    , throw
    ) where


import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Parse
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result 


-- TYPES --


data Position
    = Position
        { x :: Int
        , y :: Int
        , z :: Int
        }
        deriving (Eq)


data Part 
    = X
    | Y
    | Z



-- HELPERS --


read :: Text -> Result Error Position
read positionText =
    positionText
        & Parse.fields Parse.int
        & Result.mapError ParsingFailed
        & Result.andThen 
            (Result.mapError (MissingPart positionText) . readFields)


readFields :: Parse.Fields Int -> Result Part Position
readFields fields =
    case Parse.getField "x" fields of
        Just x ->
            case Parse.getField "y" fields of
                Just y ->
                    case Parse.getField "z" fields of
                        Just z ->
                            Ok $ Position x y z

                        Nothing ->
                            Err Z

                Nothing ->
                    Err Y

        Nothing ->
            Err X



partToText :: Part -> Text
partToText part =
    case part of
        X ->
            "x"

        Y ->
            "y"

        Z ->
            "z"


-- ERROR --


data Error
    = MissingPart Text Part
    | ParsingFailed Text


throw :: Error -> Text
throw error =
    case error of
        MissingPart text part ->
            [ "This part is missing -> "
            , partToText part
            , " from "
            , text
            ]
                & T.concat

        ParsingFailed error ->
            T.append "Parsing failed - > " error