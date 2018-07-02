module Data.Tracker
    exposing
        ( Payload
        , Tracker(..)
        )

import Css exposing (Style)
import Data.Sheet exposing (Sheet)


type Tracker
    = Big
    | Small


type alias Payload =
    { sheet : Sheet
    , majorMark : Int
    , minorMark : Int
    , fontStyle : Style
    , cellWidth : Float
    , cellHeight : Float
    }
