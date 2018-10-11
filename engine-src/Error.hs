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
import Flow
import Prelude.Extra (List)


data Error
    = UnexpectedChunkStructure (List Text)
    | VoiceError Voice.Error
    | ScoreError Note.Error


throw :: Error -> Text
throw error =
    case error of
        ScoreError err ->
            Note.throw err

        VoiceError err ->
            Voice.throw err

        UnexpectedChunkStructure chunks ->
            [ "I could not parse the score. \
              \The chunks werent what I expected. : ->\n\n"
            , T.intercalate "chunk\n\n" chunks
            ]
                |> T.concat