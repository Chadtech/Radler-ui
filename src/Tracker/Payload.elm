module Tracker.Payload
    exposing
        ( Payload
        )

import Data.Sheet exposing (Sheet)


type alias Payload =
    { sheet : Sheet
    , majorMark : Int
    , minorMark : Int
    }
