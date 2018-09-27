{-# LANGUAGE OverloadedStrings #-}


module Model 
    ( Model
    , score
    , init
    , testTxt
    , setTestTxt
    )
    where


import Data.Score (Score)
import qualified Data.Score as Score
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (init)


data Model
    = Model 
        { score :: Maybe Score 
        , testTxt :: Text
        }


init :: Model
init =
    Model
        { score = Nothing
        , testTxt = "Testy text" 
        }    


setTestTxt :: Text -> Model -> Model
setTestTxt t m =
    m { testTxt = t }