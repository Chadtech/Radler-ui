{-# LANGUAGE OverloadedStrings #-}


module Model 
    ( Model(..)
    , score
    , init
    )
    where


import Audio (Audio)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Prelude hiding (init)
import Score (Score)
import qualified Score


data Model
    = Init
    | HasScore Score Audio


instance Show Model where
    show model =
        case model of
            Init ->
                "Model.Init"

            HasScore _ _ ->
                "Model.HasScore"
            

init :: Model
init =
    Init   


score :: Model -> Maybe Score
score model =
    case model of
        Init ->
            Nothing

        HasScore score _ ->
            Just score
            

