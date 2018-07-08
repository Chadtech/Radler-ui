module Data.Tracker
    exposing
        ( Tracker
        , addToggledColumn
        , clearToggledColumns
        , closeDetails
        , init
        , openDetails
        , removeToggledColumn
        , setSheetIndex
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
    }


init : Style.Size -> Int -> Tracker
init size sheetIndex =
    { size = size
    , toggledColumns = Set.empty
    , sheetIndex = sheetIndex
    , sheetDetails = False
    }



-- HELPERS --


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


setSheetIndex : Int -> Tracker -> Tracker
setSheetIndex index tracker =
    { tracker | sheetIndex = index }


openDetails : Tracker -> Tracker
openDetails tracker =
    { tracker | sheetDetails = True }


closeDetails : Tracker -> Tracker
closeDetails tracker =
    { tracker | sheetDetails = False }
