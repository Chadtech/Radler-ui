module Model exposing
    ( Model
    , addNewPart
    , clearModal
    , clearPartsPage
    , copyPart
    , deletePart
    , fullScore
    , getPart
    , getTrackersOptions
    , getTrackersPart
    , getTrackersPartIndex
    , indexedPartNames
    , init
    , initPartsPage
    , mapPackage
    , mapPart
    , mapSelectedPart
    , mapTracker
    , removeTracker
    , saveParts
    , score
    , setBackendStatusIdle
    , setBackendStatusWorking
    , setBuildModal
    , setDeletePartModal
    , setError
    , setModal
    , setPage
    , setPartsPage
    , setPlayFor
    , setPlayFrom
    )

import Api
import Array exposing (Array)
import BackendStatus as BackendStatus exposing (BackendStatus)
import Data.Error exposing (Error(..))
import Data.Flags exposing (Flags)
import Data.Modal as Modal exposing (Modal)
import Data.Modal.Build as Build
import Data.Modal.DeletePart as DeletePart
import Data.Package as Package exposing (Package)
import Data.Page as Page exposing (Page)
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker exposing (Tracker)
import Data.Tracker.Options as Options
import Page.Parts.Model as Parts
import Style
import Util.Array as ArrayUtil



-- TYPES --


{-|

    This whole app is basically spreadsheet
    software. A Part is basically a sheet,
    and it contains an Array of Beats. A Beat
    basically a Row and it contains an Array
    of Notes. A Note is just a cell. When
    refering to a Column of Notes, its called
    a Voice

    Trackers are views that show a part.
    The number of trackers and parts are
    both dynamic. The user can have multiple
    trackers showing the same part simultaneously.

-}
type alias Model =
    { parts : Array Part
    , trackers : Array Tracker
    , page : Page
    , package : Package
    , playFromBeatField : String
    , playForBeatsField : String
    , playFromBeat : Int
    , playForBeats : Int
    , endpoints : Api.Endpoints
    , modal : Maybe Modal
    , backendStatus : BackendStatus
    }


init : Flags -> Model
init flags =
    let
        playFromBeat : Int
        playFromBeat =
            0

        playForBeats : Int
        playForBeats =
            32
    in
    { parts = flags.parts
    , trackers =
        [ Tracker.init Style.Small 0
        , Tracker.init Style.Big 0
        ]
            |> Array.fromList

    -- , page = Page.Trackers
    , page = Page.Parts Nothing
    , package = flags.package
    , modal = Nothing
    , playFromBeatField = String.fromInt playFromBeat
    , playForBeatsField = String.fromInt playForBeats
    , playFromBeat = playFromBeat
    , playForBeats = playForBeats
    , endpoints = flags.endpoints
    , backendStatus = BackendStatus.Idle
    }



-- HELPERS --


deletePart : Int -> Model -> Model
deletePart index model =
    { model | parts = ArrayUtil.remove index model.parts }


setPage : Page -> Model -> Model
setPage page model =
    { model | page = page }


setPartsPage : Parts.Model -> Model -> Model
setPartsPage =
    setPage << Page.Parts << Just


clearPartsPage : Model -> Model
clearPartsPage model =
    case model.page of
        Page.Parts (Just _) ->
            setPage (Page.Parts Nothing) model

        _ ->
            model


initPartsPage : Parts.Flags -> Model -> Model
initPartsPage =
    setPartsPage << Parts.init


indexedPartNames : Model -> List ( Int, String )
indexedPartNames { parts } =
    parts
        |> Array.map .name
        |> Array.toIndexedList


setError : Error -> Model -> Model
setError =
    setModal << Modal.Error


setModal : Modal -> Model -> Model
setModal modal model =
    { model | modal = Just modal }


setDeletePartModal : DeletePart.Model -> Model -> Model
setDeletePartModal deletePartModel =
    setModal <| Modal.DeletePart deletePartModel


setBuildModal : Build.Model -> Model -> Model
setBuildModal buildModel =
    buildModel
        |> Modal.BuildConfirmation
        |> setModal


setBackendStatusWorking : Model -> Model
setBackendStatusWorking model =
    { model | backendStatus = BackendStatus.Working }


setBackendStatusIdle : Model -> Model
setBackendStatusIdle model =
    { model | backendStatus = BackendStatus.Idle }


clearModal : Model -> Model
clearModal model =
    { model | modal = Nothing }


mapPackage : (Package -> Package) -> Model -> Model
mapPackage f model =
    { model | package = f model.package }


mapPart : Int -> (Part -> Part) -> Model -> Model
mapPart index f model =
    case getPart index model of
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


mapSelectedPart : Maybe Parts.Model -> (Part -> Part) -> Model -> Model
mapSelectedPart maybePartsModel f model =
    case maybePartsModel of
        Just partsModel ->
            mapPart
                partsModel.selectedPartIndex
                f
                model

        Nothing ->
            model


getPart : Int -> Model -> Maybe Part
getPart index model =
    Array.get index model.parts


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
            ArrayUtil.remove index model.trackers
    }


getTracker : Int -> Model -> Maybe Tracker
getTracker trackerIndex model =
    Array.get trackerIndex model.trackers


getTrackersOptions : Int -> Model -> Maybe Options.Model
getTrackersOptions trackerIndex =
    getTracker trackerIndex >> Maybe.andThen .options


getTrackersPartIndex : Int -> Model -> Maybe Int
getTrackersPartIndex trackerIndex =
    getTracker trackerIndex >> Maybe.map .partIndex


getTrackersPart : Int -> Model -> Maybe Part
getTrackersPart trackerIndex model =
    case getTrackersPartIndex trackerIndex model of
        Just partIndex ->
            Array.get partIndex model.parts

        Nothing ->
            Nothing


setPlayFrom : String -> Model -> Model
setPlayFrom str model =
    case String.toInt str of
        Just fromBeat ->
            { model
                | playFromBeatField = str
                , playFromBeat = fromBeat
            }

        Nothing ->
            { model
                | playFromBeatField =
                    str
            }


setPlayFor : String -> Model -> Model
setPlayFor str model =
    case String.toInt str of
        Just fromBeat ->
            { model
                | playForBeatsField = str
                , playForBeats = fromBeat
            }

        Nothing ->
            { model
                | playForBeatsField =
                    str
            }


copyPart : Int -> String -> Model -> Model
copyPart partIndex copysName model =
    case Array.get partIndex model.parts of
        Just part ->
            { model
                | parts =
                    Array.push
                        { part | name = copysName }
                        model.parts
            }

        Nothing ->
            model


addNewPart : Model -> Model
addNewPart model =
    let
        addNewPartWithName : String -> Model
        addNewPartWithName newPartName =
            if
                model.parts
                    |> Array.map .name
                    |> Array.toList
                    |> List.member newPartName
            then
                addNewPartWithName (newPartName ++ "-1")

            else
                { model
                    | parts =
                        Array.push
                            (Part.empty newPartName)
                            model.parts
                }
    in
    addNewPartWithName "new-part"



-- SAVING -


{-|

    Save the human-readable-ish parts
    to the disk. These are basically
    just csv files that contain strings
    of whats in the UI.

-}
saveParts : Model -> Cmd msg
saveParts model =
    model.parts
        |> Array.toList
        |> List.map Part.saveToDisk
        |> Cmd.batch


{-|

    If we save this project and it works
    we want a 'Cmd msg' that will execute
    the save. If we save this project and
    it failed, we want a new 'Model' which
    contains the fail information that should
    be displayed in the UI

    saving the score is like saving parts
    (read the parts documentation above),
    with a few exceptions
    0 The score is just one file that is
    constructed from the parts (see the
    documantion in Data/Package.elm for more
    information)
    1 The Score contains timing information
    for each note. Parts dont contain any
    timing information

-}
score : Model -> Result Model String
score model =
    scoreHelper
        model
        { package = model.package
        , parts = model.parts
        , from = model.playFromBeat
        , length = Just model.playForBeats
        }


fullScore : Model -> Result Model String
fullScore model =
    scoreHelper
        model
        { package = model.package
        , parts = model.parts
        , from = 0
        , length = Nothing
        }


scoreHelper : Model -> Package.ScoreParams -> Result Model String
scoreHelper model scoreParams =
    case Package.scorePayload scoreParams of
        Just scoreStr ->
            Ok scoreStr

        Nothing ->
            model
                |> setError ScoreDidNotSave
                |> Err
