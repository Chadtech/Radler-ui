{-# LANGUAGE OverloadedStrings #-}


module Position
    ( Position
    , x
    , y
    , z
    , fromCoords
    , addToX
    , setX
    , distanceBetween
    , Position.read
    , Error
    , throw
    ) where


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
        { x :: Float
        , y :: Float
        , z :: Float
        }
        deriving (Eq)


data Part 
    = X
    | Y
    | Z



-- HELPERS --


fromCoords :: (Float, Float, Float) -> Position
fromCoords (x, y, z) =
    Position x y z

    
read :: Parse.Fields Float -> Result Error Position
read roomFields =
    Result.mapError MissingPart (readFields roomFields)


readFields :: Parse.Fields Float -> Result Part Position
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


addToX :: Float -> Position -> Position
addToX deltaX position =
    position { x = (x position) + deltaX }


setX :: Float -> Position -> Position
setX newX position =
    position { x = newX }


distanceBetween :: Position -> Position -> Float
distanceBetween p0 p1 =
    sqrt (((x p0 - x p1) ^ 2) + ((y p0 - y p1) ^ 2) + ((z p0 - z p1) ^ 2))


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