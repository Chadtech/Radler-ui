{-# LANGUAGE OverloadedStrings #-}


module Error
    ( Error(..) 
    , throw
    )
    where


import Flow

import qualified Score
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Terminal


data Error
    = ScoreError Score.Error
    | TerminalError Terminal.Error


instance Show Error where
    show = T.unpack <. throw 
    

throw :: Error -> Text
throw err =
    case err of
        ScoreError scoreErr ->
            Score.throw scoreErr

        TerminalError terminalErr ->
            Terminal.throw terminalErr