module Tracker
    exposing
        ( Msg(..)
        , Payload
        )

import Data.Sheet exposing (Sheet)


type Msg
    = Noop


type alias Payload =
    { sheet : Sheet
    }
