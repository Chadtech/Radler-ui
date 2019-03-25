module Data.Modal exposing
    ( Modal(..)
    , getDeleteModel
    , initBuild
    )

import Data.Error exposing (Error)
import Data.Modal.Build as Build
import Data.Modal.DeletePart as DeletePart



-- TYPES --


type Modal
    = Error Error
    | BuildConfirmation Build.Model
    | DeletePart DeletePart.Model



-- HELPERS --


initBuild : Modal
initBuild =
    BuildConfirmation Build.Ready


getDeleteModel : Modal -> Maybe DeletePart.Model
getDeleteModel modal =
    case modal of
        DeletePart model ->
            Just model

        _ ->
            Nothing
