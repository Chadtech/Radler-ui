module Model
    exposing
        ( Model
        , empty
        , getThreadsSheetIndex
        , mapSheet
        )

import Array exposing (Array)
import Data.Sheet as Sheet exposing (Sheet)
import Data.Tracker exposing (Tracker(..))


type alias Model =
    { projectName : String
    , sheets : Array Sheet
    , trackers : Array ( Int, Tracker )
    , majorMark : Int
    , minorMark : Int
    }


empty : Model
empty =
    { projectName = ""
    , sheets = Array.fromList [ Sheet.empty ]
    , trackers =
        [ ( 0, Small )
        , ( 0, Big )
        , ( 0, Big )
        , ( 0, Small )
        ]
            |> Array.fromList
    , majorMark = 16
    , minorMark = 4
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


getThreadsSheetIndex : Int -> Model -> Maybe Int
getThreadsSheetIndex threadIndex model =
    Array.get threadIndex model.trackers
        |> Maybe.map Tuple.first
