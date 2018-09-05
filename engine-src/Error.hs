module Error
    ( Error(..) 
    , throw
    )
    where

import qualified Data.Project as Project
import qualified Data.Part as Part
import qualified Data.Voice as Voice


data Error
    = UnexpectedChunkStructure
    | VoiceError Voice.Error


throw :: Error -> String
throw error =
    case error of
        VoiceError err ->
            Voice.throw err

        UnexpectedChunkStructure ->
            "I could not parse the score. The chunks werent what I expected."