{-# LANGUAGE OverloadedStrings #-}


module Size
  ( Size
  , fromFloatFields
  , height
  , Size.length
  , multiplyBy
  , width
  , Error
  , throw
  )
where


import           Flow
import           Prelude.Extra

import qualified Data.Either.Extra             as Either
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Parse


-- TYPES --


data Size
    = Size
        { width :: Float
        , length :: Float
        , height :: Float
        }
        deriving (Eq)


instance Show Size where
  show position =
    [ T.pack ("width : " ++ show (width position))
      , T.pack ("length : " ++ show (Size.length position))
      , T.pack ("height : " ++ show (height position))
      ]
      |> T.concat
      |> T.unpack


data Dimension
    = Width
    | Length
    | Height
    deriving (Eq)


-- HELPERS --


fromFloatFields :: Parse.Fields Float -> Either Error Size
fromFloatFields = Either.mapLeft MissingDimension <. fromFieldsHelper


fromFieldsHelper :: Parse.Fields Float -> Either Dimension Size
fromFieldsHelper fields = case Parse.get "width" fields of
  Just width -> case Parse.get "length" fields of
    Just length -> case Parse.get "height" fields of
      Just height -> Size width length height |> Right

      Nothing     -> Left Height

    Nothing -> Left Length

  Nothing -> Left Width


dimensionToText :: Dimension -> Text
dimensionToText dimension = case dimension of
  Width  -> "width"

  Length -> "length"

  Height -> "height"


multiplyBy :: Int -> Size -> Size
multiplyBy factor size = Size (toFloat factor * width size)
                              (toFloat factor * Size.length size)
                              (toFloat factor * height size)


-- ERROR --


data Error
    = MissingDimension Dimension
    deriving (Eq)


throw :: Error -> Text
throw error = case error of
  MissingDimension dimension ->
    ["This part is missing -> ", dimensionToText dimension] |> T.concat
