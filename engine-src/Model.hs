{-# LANGUAGE OverloadedStrings #-}


module Model 
    ( Model
    , score
    , init
    , setScore
    )
    where


import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Prelude hiding (init)
import Score (Score)
import qualified Score


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

