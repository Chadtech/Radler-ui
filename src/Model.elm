module Model
    exposing
        ( Model
        , Page(..)
        , apply
        , getThreadsSheetIndex
        , init
        , mapPackage
        , mapSheet
        , mapTracker
        , removeTracker
        , save
        )

import Array exposing (Array)
import Data.Flags as Flags exposing (Flags)
import Data.Package exposing (Package)
import Data.Sheet as Sheet exposing (Sheet)
import Data.Tracker as Tracker
    exposing
        ( Tracker
        )
import Ports
import Style


-- TYPES --


type alias Model =
    { sheets : Array Sheet
    , trackers : Array Tracker
    , page : Page
    , package : Package
    }


type Page
    = Package
    | Trackers


init : Flags -> Model
init flags =
    { sheets =
        [ Sheet.empty
        , Sheet.empty
        , Sheet.empty
        ]
            |> Array.fromList
    , trackers =
        [ Tracker.init Style.Small 0
        ]
            |> Array.fromList
    , page = Trackers
    , package = flags.package
    }



-- HELPERS --


apply : Model -> (Model -> Model) -> Model
apply model f =
    f model


mapPackage : (Package -> Package) -> Model -> Model
mapPackage f model =
    { model | package = f model.package }


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


save : Model -> Cmd msg
save model =
    [ model.sheets
        |> Array.toList
        |> List.map saveSheet
        |> Cmd.batch

    -- , model.package
    -- |> savePackage
    ]
        |> Cmd.batch


savePackage : String -> Cmd msg
savePackage =
    Ports.SavePackage >> Ports.send


saveSheet : Sheet -> Cmd msg
saveSheet sheet =
    sheet
        |> Sheet.toFile
        |> Ports.SaveSheet
        |> Ports.send
