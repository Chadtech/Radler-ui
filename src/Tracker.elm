module Tracker
    exposing
        ( Payload
        )

import Data.Sheet exposing (Sheet)


type alias Payload =
    { sheet : Sheet
    }
