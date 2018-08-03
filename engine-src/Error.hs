module Error
    ( Error(..) 
    , throw
    )
    where

import qualified Data.Project as Project
import qualified Data.Part as Part
import qualified Data.Voice as Voice


data Error
    = ProjectError Project.Error



throw :: Error -> String
throw error =
    case error of
        ProjectError subError ->
            Project.throw subError