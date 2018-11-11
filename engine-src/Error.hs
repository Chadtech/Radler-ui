{-# LANGUAGE OverloadedStrings #-}


module Error
    ( Error(..) 
    , throw
    )
    where


import qualified Score
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Prelude.Extra (List)


data Error
    = ScoreError Score.Error


throw :: Error -> Text
throw error =
    case error of
        ScoreError err ->
            Score.throw err
