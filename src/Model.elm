module Model
    exposing
        ( Model
        , Page(..)
        , empty
        , getThreadsSheetIndex
        , mapSheet
        , mapTracker
        , removeTracker
        )

import Array exposing (Array)
import Data.Sheet as Sheet exposing (Sheet)
import Data.Tracker as Tracker
    exposing
        ( Tracker
        )
import Style


-- TYPES --


type alias Model =
    { projectName : String
    , sheets : Array Sheet
    , trackers : Array Tracker
    , majorMark : Int
    , minorMark : Int
    , page : Page
    }


type Page
    = Package
    | Trackers


empty : Model
empty =
    { projectName = ""
    , sheets =
        [ Sheet.empty
        , Sheet.empty
        , Sheet.empty
        ]
            |> Array.fromList
    , trackers =
        [ Tracker.init Style.Small 0
        , Tracker.init Style.Big 0
        , Tracker.init Style.Big 0
        , Tracker.init Style.Small 0
        ]
            |> Array.fromList
    , majorMark = 16
    , minorMark = 4
    , page = Trackers
    }



-- HELPERS --


mapSheet : Int -> (Sheet -> Sheet) -> Model -> Model
mapSheet index f model =
    case Array.get index model.sheets of
        Just sheet ->
            { model
                | sheets =
                    Array.set
                        index
                        (f sheet)
                        model.sheets
            }

        Nothing ->
            model


mapTracker : Int -> (Tracker -> Tracker) -> Model -> Model
mapTracker index f model =
    case Array.get index model.trackers of
        Just tracker ->
            { model
                | trackers =
                    Array.set
                        index
                        (f tracker)
                        model.trackers
            }

        Nothing ->
            model


removeTracker : Int -> Model -> Model
removeTracker index model =
    { model
        | trackers =
            model.trackers
                |> Array.slice
                    (index + 1)
                    (Array.length model.trackers)
                |> Array.append
                    (Array.slice 0 index model.trackers)
    }


getThreadsSheetIndex : Int -> Model -> Maybe Int
getThreadsSheetIndex threadIndex model =
    model.trackers
        |> Array.get threadIndex
        |> Maybe.map .sheetIndex
