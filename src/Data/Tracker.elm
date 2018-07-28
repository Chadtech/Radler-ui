module Data.Tracker
    exposing
        ( Tracker
        , closeDetails
        , init
        , openDetails
        , setMajorMark
        , setMinorMark
        , setPartIndex
        , setSize
        )

import Css exposing (Style)
import Style


-- TYPES --


type alias Tracker =
    { size : Style.Size
    , partIndex : Int
    , partDetails : Bool
    , majorMark : Int
    , minorMark : Int
    }


init : Style.Size -> Int -> Tracker
init size partIndex =
    { size = size
    , partIndex = partIndex
    , partDetails = False
    , majorMark = 16
    , minorMark = 4
    }



-- HELPERS --


setMajorMark : Int -> Tracker -> Tracker
setMajorMark majorMark tracker =
    { tracker | majorMark = majorMark }


setMinorMark : Int -> Tracker -> Tracker
setMinorMark minorMark tracker =
    { tracker | minorMark = minorMark }


setSize : Style.Size -> Tracker -> Tracker
setSize size tracker =
    { tracker | size = size }


setPartIndex : Int -> Tracker -> Tracker
setPartIndex index tracker =
    { tracker | partIndex = index }


openDetails : Tracker -> Tracker
openDetails tracker =
    { tracker | partDetails = True }


closeDetails : Tracker -> Tracker
closeDetails tracker =
    { tracker | partDetails = False }
