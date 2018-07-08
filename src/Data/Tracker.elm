module Data.Tracker
    exposing
        ( Tracker
        , closeDetails
        , init
        , openDetails
        , setSheetIndex
        )

import Css exposing (Style)
import Style


-- TYPES --


type alias Tracker =
    { size : Style.Size
    , sheetIndex : Int
    , sheetDetails : Bool
    }


init : Style.Size -> Int -> Tracker
init size sheetIndex =
    { size = size
    , sheetIndex = sheetIndex
    , sheetDetails = False
    }



-- HELPERS --


setSheetIndex : Int -> Tracker -> Tracker
setSheetIndex index tracker =
    { tracker | sheetIndex = index }


openDetails : Tracker -> Tracker
openDetails tracker =
    { tracker | sheetDetails = True }


closeDetails : Tracker -> Tracker
closeDetails tracker =
    { tracker | sheetDetails = False }
