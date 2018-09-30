{-# LANGUAGE OverloadedStrings #-}


module Model 
    ( Model
    , score
    , init
    , setScore
    )
    where


import Data.Score (Score)
import qualified Data.Score as Score
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Prelude hiding (init)


data Model
    = Model 
        { score :: Maybe Score 
        }


init :: Model
init =
    Model
        { score = Nothing }    


setScore :: Score -> Model -> Model
setScore score m =
    m { score = Just score }