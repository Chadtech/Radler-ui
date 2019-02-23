module Data.Tracker exposing
    ( Tracker
    , closeOptions
    , init
    , mapOptionsModel
    , openOptions
    , setMajorMark
    , setMinorMark
    , setPartIndex
    , setSize
    )

import Data.Tracker.Options as Options
import Style



-- TYPES --


{-|

    Tracker :=
        You know how music is read from left
        to right? Where the left notes happen
        earlier than the right notes? In the
        olden times of making music with computers,
        a musical interface was used called a
        'tracker', where notes were input into
        a spreadsheet with the earlier notes
        being put in the top rows. The music
        was played from top to bottom by the
        computer after the composer entered the
        right notes into the spread sheet.

        Thats what this software is, tracker
        software. This software as a whole
        manages several tracker UIs. Each tracker
        is like an independent view into the
        singular musical score the whole software
        is modifying. Each tracker is showing a
        specific part of music ('partIndex').
        It shows it in a certain size ('size').
        There are major and minor marks, which
        represent rythmically important information
        in the form of certain rows being
        highlighted.

-}
type alias Tracker =
    { size : Style.Size
    , partIndex : Int
    , options : Maybe Options.Model
    , majorMark : Int
    , minorMark : Int
    }


init : Style.Size -> Int -> Tracker
init size partIndex =
    { size = size
    , partIndex = partIndex
    , options = Nothing
    , majorMark = 16
    , minorMark = 4
    }



-- HELPERS --


setMajorMark : String -> Tracker -> Tracker
setMajorMark majorMarkString tracker =
    let
        fieldUpdatedTracker =
            mapOptionsModel
                (Options.setMajorMarkField majorMarkString)
                tracker
    in
    case String.toInt majorMarkString of
        Just majorMark ->
            { fieldUpdatedTracker | majorMark = majorMark }

        Nothing ->
            fieldUpdatedTracker


setMinorMark : String -> Tracker -> Tracker
setMinorMark minorMarkString tracker =
    let
        fieldUpdatedTracker =
            mapOptionsModel
                (Options.setMinorMarkField minorMarkString)
                tracker
    in
    case String.toInt minorMarkString of
        Just minorMark ->
            { fieldUpdatedTracker | minorMark = minorMark }

        Nothing ->
            fieldUpdatedTracker


setSize : Style.Size -> Tracker -> Tracker
setSize size tracker =
    { tracker | size = size }


setPartIndex : Int -> Tracker -> Tracker
setPartIndex index tracker =
    { tracker | partIndex = index }


mapOptionsModel : (Options.Model -> Options.Model) -> Tracker -> Tracker
mapOptionsModel f tracker =
    { tracker | options = Maybe.map f tracker.options }


openOptions : String -> Tracker -> Tracker
openOptions partName tracker =
    { tracker
        | options =
            { majorMarkField =
                String.fromInt tracker.majorMark
            , minorMarkField =
                String.fromInt tracker.minorMark
            , copyName = partName ++ "-copy"
            }
                |> Just
    }


closeOptions : Tracker -> Tracker
closeOptions tracker =
    { tracker | options = Nothing }
