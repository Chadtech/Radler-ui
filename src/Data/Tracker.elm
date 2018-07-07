module Data.Tracker
    exposing
        ( Tracker
        , closeDetails
        , init
        , mapDetails
        , openDetails
        )

import Css exposing (Style)
import Details
import Style


-- TYPES --


type alias Tracker =
    { size : Style.Size
    , sheetIndex : Int
    , sheetDetails : Maybe Details.Model
    }


init : Style.Size -> Int -> Tracker
init size sheetIndex =
    { size = size
    , sheetIndex = sheetIndex
    , sheetDetails = Nothing
    }



-- HELPERS --


openDetails : String -> Tracker -> Tracker
openDetails sheetName tracker =
    { tracker
        | sheetDetails =
            { sheetNameField = sheetName }
                |> Just
    }


closeDetails : Tracker -> Tracker
closeDetails tracker =
    { tracker | sheetDetails = Nothing }


mapDetails : (Details.Model -> Details.Model) -> Tracker -> Tracker
mapDetails f tracker =
    { tracker | sheetDetails = Maybe.map f tracker.sheetDetails }
