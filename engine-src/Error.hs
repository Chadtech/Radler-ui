{-# LANGUAGE OverloadedStrings #-}


module Error
    ( Error(..) 
    , throw
    )
    where


import qualified Data.Note as Note
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Voice as Voice


data Error
    = UnexpectedChunkStructure
    | VoiceError Voice.Error
    | ScoreError Note.Error


throw :: Error -> Text
throw error =
    case error of
        ScoreError err ->
            Note.throw err

        VoiceError err ->
            Voice.throw err

        UnexpectedChunkStructure ->
            "I could not parse the score. The chunks werent what I expected."