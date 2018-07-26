module Model
    exposing
        ( Model
        , Page(..)
        , apply
        , getTrackersPartIndex
        , init
        , mapPackage
        , mapPart
        , mapTracker
        , removeTracker
        , saveToDisk
        )

import Array exposing (Array)
import Data.Flags as Flags exposing (Flags)
import Data.Package as Package exposing (Package)
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker
    exposing
        ( Tracker
        )
import Ports
import Style


-- TYPES --


type alias Model =
    { parts : Array Part
    , trackers : Array Tracker
    , page : Page
    , package : Package
    }


type Page
    = Package
    | Trackers


init : Flags -> Model
init flags =
    { parts = flags.parts
    , trackers =
        [ Tracker.init Style.Small 0 ]
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


mapPart : Int -> (Part -> Part) -> Model -> Model
mapPart index f model =
    case Array.get index model.parts of
        Just part ->
            { model
                | parts =
                    Array.set
                        index
                        (f part)
                        model.parts
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


getTrackersPartIndex : Int -> Model -> Maybe Int
getTrackersPartIndex threadIndex model =
    model.trackers
        |> Array.get threadIndex
        |> Maybe.map .partIndex


saveToDisk : Model -> Cmd msg
saveToDisk model =
    [ model.parts
        |> Array.toList
        |> List.map Part.saveToDisk
        |> Cmd.batch
    , model.package
        |> Package.saveToDisk
    ]
        |> Cmd.batch
