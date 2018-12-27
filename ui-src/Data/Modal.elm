module Data.Modal exposing (Modal(..))

import Data.Error exposing (Error)


type Modal
    = Error Error
    | BuildConfirmation
