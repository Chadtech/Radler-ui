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
  )
where


import           Prelude.Extra
import           Flow

import qualified Data.Either.Extra             as Either
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Parse


-- TYPES --


data Position
    = Position
        { x :: Float
        , y :: Float
        , z :: Float
        }
        deriving (Eq)


instance Show Position where
  show position =
    [ T.pack ("x : " ++ show (x position))
      , T.pack ("y : " ++ show (y position))
      , T.pack ("z : " ++ show (z position))
      ]
      |> T.concat
      |> T.unpack



data Part
    = X
    | Y
    | Z
    deriving (Eq)


-- HELPERS --


fromCoords :: (Float, Float, Float) -> Position
fromCoords (x, y, z) = Position x y z


read :: Parse.Fields Float -> Either Error Position
read roomFields = Either.mapLeft MissingPart <| readFields roomFields


readFields :: Parse.Fields Float -> Either Part Position
readFields fields = case Parse.get "x" fields of
  Just x -> case Parse.get "y" fields of
    Just y -> case Parse.get "z" fields of
      Just z  -> Right <| Position x y z

      Nothing -> Left Z

    Nothing -> Left Y

  Nothing -> Left X


partToText :: Part -> Text
partToText part = case part of
  X -> "x"

  Y -> "y"

  Z -> "z"


addToX :: Float -> Position -> Position
addToX deltaX position = position { x = (x position) + deltaX }


setX :: Float -> Position -> Position
setX newX position = position { x = newX }


distanceBetween :: Position -> Position -> Float
distanceBetween p0 p1 =
  sqrt (((x p0 - x p1) ^ 2) + ((y p0 - y p1) ^ 2) + ((z p0 - z p1) ^ 2))


-- ERROR --


data Error
    = MissingPart Part
    deriving (Eq)


throw :: Error -> Text
throw error = case error of
  MissingPart part -> ["This part is missing -> ", partToText part] |> T.concat
