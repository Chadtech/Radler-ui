module Data.Tracker
    exposing
        ( Tracker
        , closeOptions
        , init
        , openOptions
        , setMajorMark
        , setMinorMark
        , setPartIndex
        , setSize
        )

import Css exposing (Style)
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
        manages several tracker UIs, but each
        one certain UI information. Each
        tracker is showing a specific part of
        music ('partIndex'). It shows it in a
        certain size ('size'). There are major
        and minor marks, which can represent
        rythmically important information in
        the form of certain rows being
        highlighted

-}
type alias Tracker =
    { size : Style.Size
    , partIndex : Int
    , partOptions : Bool
    , majorMark : Int
    , minorMark : Int
    }


init : Style.Size -> Int -> Tracker
init size partIndex =
    { size = size
    , partIndex = partIndex
    , partOptions = False
    , majorMark = 16
    , minorMark = 4
    }



-- HELPERS --


setMajorMark : Int -> Tracker -> Tracker
setMajorMark majorMark tracker =
    { tracker | majorMark = majorMark }


setMinorMark : Int -> Tracker -> Tracker
setMinorMark minorMark tracker =
    { tracker | minorMark = minorMark }


setSize : Style.Size -> Tracker -> Tracker
setSize size tracker =
    { tracker | size = size }


setPartIndex : Int -> Tracker -> Tracker
setPartIndex index tracker =
    { tracker | partIndex = index }


openOptions : Tracker -> Tracker
openOptions tracker =
    { tracker | partOptions = True }


closeOptions : Tracker -> Tracker
closeOptions tracker =
    { tracker | partOptions = False }
