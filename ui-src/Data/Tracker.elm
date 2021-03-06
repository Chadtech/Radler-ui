module Data.Tracker exposing
    ( Tracker
    , closeOptions
    , decoder
    , encode
    , init
    , mapOptions
    , openOptions
    , setCollapse
    , setMajorMark
    , setMinorMark
    , setPartIndex
    , setSize
    )

import Data.Index as Index exposing (Index)
import Data.Part exposing (Part)
import Data.Size as Size exposing (Size)
import Data.Tracker.Collapse as Collapse exposing (Collapse)
import Data.Tracker.Options as TrackerOptions
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



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

        Thats what this software is; tracker
        software. This software as a whole
        manages several tracker UIs. Each tracker
        is like an independent view into the
        singular musical score the whole software
        is modifying. Each tracker is showing a
        specific part of music ('partIndex').
        It be rendered in a different sizes ('size').
        There are major and minor marks, which
        show up as certain rows behind highlighted
        in different colors, which conveys
        rythmically important information.
        highlighted.

-}
type alias Tracker =
    { size : Size
    , partIndex : Index Part
    , majorMark : Int
    , minorMark : Int
    , collapse : Collapse
    , options : Maybe TrackerOptions.Model
    }



-- INIT --


init : Size -> Index Part -> Tracker
init size partIndex =
    { size = size
    , partIndex = partIndex
    , majorMark = 16
    , minorMark = 4
    , collapse = Collapse.none
    , options = Nothing
    }



-- HELPERS --


mapOptions : (TrackerOptions.Model -> TrackerOptions.Model) -> Tracker -> Tracker
mapOptions mapper tracker =
    { tracker
        | options =
            Maybe.map mapper tracker.options
    }


setMajorMark : Int -> Tracker -> Tracker
setMajorMark newMajorMark tracker =
    { tracker | majorMark = newMajorMark }


setMinorMark : Int -> Tracker -> Tracker
setMinorMark newMinorMark tracker =
    { tracker | minorMark = newMinorMark }


setSize : Size -> Tracker -> Tracker
setSize size tracker =
    { tracker | size = size }


setPartIndex : Index Part -> Tracker -> Tracker
setPartIndex index tracker =
    { tracker | partIndex = index }


openOptions : Tracker -> Tracker
openOptions tracker =
    let
        options : TrackerOptions.Model
        options =
            tracker.collapse
                |> Collapse.getEveryAmount
                |> Maybe.withDefault 4
                |> TrackerOptions.init
    in
    { tracker | options = Just options }


closeOptions : Tracker -> Tracker
closeOptions tracker =
    { tracker | options = Nothing }


setCollapse : Collapse -> Tracker -> Tracker
setCollapse collapse tracker =
    { tracker | collapse = collapse }


encode : Tracker -> Encode.Value
encode tracker =
    [ ( "size", Size.encode tracker.size )
    , ( "partIndex", Index.encode tracker.partIndex )
    , ( "majorMark", Encode.int tracker.majorMark )
    , ( "minorMark", Encode.int tracker.minorMark )
    , ( "collapse", Collapse.encode tracker.collapse )
    ]
        |> Encode.object


decoder : Decoder Tracker
decoder =
    Decode.map6 Tracker
        (Decode.field "size" Size.decoder)
        (Decode.field "partIndex" Index.decoder)
        (Decode.field "majorMark" Decode.int)
        (Decode.field "minorMark" Decode.int)
        (Decode.field "collapse" Collapse.decoder)
        (Decode.succeed Nothing)
