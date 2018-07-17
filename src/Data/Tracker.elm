module Data.Tracker
    exposing
        ( Tracker
        , addToggledColumn
        , clearToggledColumns
        , closeDetails
        , init
        , openDetails
        , removeToggledColumn
        , setMajorMark
        , setMinorMark
        , setSheetIndex
        , setSize
        )

import Css exposing (Style)
import Set exposing (Set)
import Style


-- TYPES --


type alias Tracker =
    { size : Style.Size
    , toggledColumns : Set Int
    , sheetIndex : Int
    , sheetDetails : Bool
    , majorMark : Int
    , minorMark : Int
    }


init : Style.Size -> Int -> Tracker
init size sheetIndex =
    { size = size
    , toggledColumns = Set.empty
    , sheetIndex = sheetIndex
    , sheetDetails = False
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


addToggledColumn : Int -> Tracker -> Tracker
addToggledColumn index tracker =
    { tracker
        | toggledColumns =
            Set.insert index tracker.toggledColumns
    }


removeToggledColumn : Int -> Tracker -> Tracker
removeToggledColumn index tracker =
    { tracker
        | toggledColumns =
            Set.remove index tracker.toggledColumns
    }


clearToggledColumns : Tracker -> Tracker
clearToggledColumns tracker =
    { tracker | toggledColumns = Set.empty }


setSize : Style.Size -> Tracker -> Tracker
setSize size tracker =
    { tracker | size = size }


setSheetIndex : Int -> Tracker -> Tracker
setSheetIndex index tracker =
    { tracker | sheetIndex = index }


openDetails : Tracker -> Tracker
openDetails tracker =
    { tracker | sheetDetails = True }


closeDetails : Tracker -> Tracker
closeDetails tracker =
    { tracker | sheetDetails = False }
