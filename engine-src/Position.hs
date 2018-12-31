{-# LANGUAGE OverloadedStrings #-}


module Position
    ( Position
    , x
    , y
    , z
    , Position.read
    , Error
    , throw
    ) where


-- import qualified Data.Attoparsec.Text as P
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


read :: Parse.Fields Int -> Result Error Position
read roomFields =
    Result.mapError MissingPart (readFields roomFields)


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
    = MissingPart Part


throw :: Error -> Text
throw error =
    case error of
        MissingPart part ->
            [ "This part is missing -> "
            , partToText part
            ]
                & T.concat