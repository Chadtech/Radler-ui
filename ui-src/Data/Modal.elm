module Data.Modal exposing
    ( Modal(..)
    , initBuild
    )

import Data.Error exposing (Error)
import Data.Modal.Build as Build


type Modal
    = Error Error
    | BuildConfirmation Build.Model


initBuild : Modal
initBuild =
    BuildConfirmation Build.Ready
