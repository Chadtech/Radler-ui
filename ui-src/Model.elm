module Model exposing
    ( Model
    , addNewPart
    , addNewTracker
    , clearModal
    , clearPartsPage
    , copyPart
    , deletePart
    , fullScore
    , getPart
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
    , save
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
    , toggleRepeatPlayback
    )

import Api
import Array exposing (Array)
import BackendStatus as BackendStatus exposing (BackendStatus)
import Data.Error exposing (Error(..))
import Data.Flags exposing (Flags)
import Data.Index as Index exposing (Index)
import Data.Modal as Modal exposing (Modal)
import Data.Modal.Build as Build
import Data.Modal.DeletePart as DeletePart
import Data.Package as Package exposing (Package)
import Data.Page as Page exposing (Page)
import Data.Page.Parts as Parts
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker exposing (Tracker)
import Ports
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
    , repeatPlayback : Bool
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
        [ Tracker.init Style.Big Index.zero
        , Tracker.init Style.Big Index.zero
        ]
            |> Array.fromList
    , page = Page.Trackers
    , package = flags.package
    , modal = Nothing
    , playFromBeatField = String.fromInt playFromBeat
    , playForBeatsField = String.fromInt playForBeats
    , playFromBeat = playFromBeat
    , playForBeats = playForBeats
    , repeatPlayback = False
    , endpoints = flags.endpoints
    , backendStatus = BackendStatus.Idle
    }



-- HELPERS --


toggleRepeatPlayback : Model -> Model
toggleRepeatPlayback model =
    { model | repeatPlayback = not model.repeatPlayback }


deletePart : Index Part -> Model -> Model
deletePart index model =
    { model | parts = ArrayUtil.remove (Index.toInt index) model.parts }


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


indexedPartNames : Model -> List ( Index Part, String )
indexedPartNames { parts } =
    parts
        |> Index.toEntries
        |> List.map (Tuple.mapSecond .name)


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


mapPart : Index Part -> (Part -> Part) -> Model -> Model
mapPart index f model =
    case getPart index model of
        Just part ->
            { model
                | parts =
                    Array.set
                        (Index.toInt index)
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


getPart : Index Part -> Model -> Maybe Part
getPart index model =
    Array.get (Index.toInt index) model.parts


mapTracker : Index Tracker -> (Tracker -> Tracker) -> Model -> Model
mapTracker index f model =
    let
        i : Int
        i =
            Index.toInt index
    in
    case Array.get i model.trackers of
        Just tracker ->
            { model
                | trackers =
                    Array.set
                        i
                        (f tracker)
                        model.trackers
            }

        Nothing ->
            model


removeTracker : Index Tracker -> Model -> Model
removeTracker index model =
    { model
        | trackers =
            ArrayUtil.remove (Index.toInt index) model.trackers
    }


addNewTracker : Model -> Model
addNewTracker model =
    { model
        | trackers =
            Array.push
                (Tracker.init Style.Big Index.zero)
                model.trackers
    }


getTracker : Index Tracker -> Model -> Maybe Tracker
getTracker trackerIndex model =
    Array.get (Index.toInt trackerIndex) model.trackers


getTrackersPartIndex : Index Tracker -> Model -> Maybe (Index Part)
getTrackersPartIndex trackerIndex =
    getTracker trackerIndex >> Maybe.map .partIndex


getTrackersPart : Index Tracker -> Model -> Maybe Part
getTrackersPart trackerIndex model =
    case getTrackersPartIndex trackerIndex model of
        Just partIndex ->
            Array.get (Index.toInt partIndex) model.parts

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


copyPart : Index Part -> String -> Model -> Model
copyPart partIndex copysName model =
    case getPart partIndex model of
        Just part ->
            { model
                | parts =
                    Array.push
                        { part | name = copysName }
                        model.parts
            }

        Nothing ->
            model


addNewPart : Model -> ( Model, Cmd msg )
addNewPart model =
    let
        addNewPartWithName : String -> ( Model, Cmd msg )
        addNewPartWithName newPartName =
            if
                model.parts
                    |> Array.map .name
                    |> Array.toList
                    |> List.member newPartName
            then
                addNewPartWithName (newPartName ++ "-1")

            else
                let
                    newPart : Part
                    newPart =
                        Part.empty newPartName
                in
                ( { model
                    | parts =
                        Array.push
                            newPart
                            model.parts
                  }
                , Ports.savePartToDisk newPart
                )
    in
    addNewPartWithName "new-part"



-- SAVING -


{-| Save everything, parts and package
-}
save : Model -> Cmd msg
save model =
    [ Package.saveToDisk
        model.package
    , saveParts
        model
    ]
        |> Cmd.batch


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
        |> List.map Ports.savePartToDisk
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
