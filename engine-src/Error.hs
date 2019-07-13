{-# LANGUAGE OverloadedStrings #-}


module Error
  ( Error(..)
  , throw
  )
where


import           Flow

import qualified Score
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import           Prelude.Extra                  ( List )


data Error
    = ScoreError Score.Error


instance Show Error where
  show = T.unpack <. throw


throw :: Error -> Text
throw error = case error of
  ScoreError err -> Score.throw err
