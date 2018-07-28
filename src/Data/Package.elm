module Data.Package
    exposing
        ( Package
        , decoder
        , saveScoreToDisk
        , saveToDisk
        , setJsonStrField
        )

import Array exposing (Array)
import Data.Beat as Beat exposing (Beat)
import Data.Note as Note exposing (Note)
import Data.Part as Part exposing (Part)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Ports
import Random exposing (Seed)
import Util


-- TYPES --


{-|

    This package is mostly used to turn
    the parts into a musical score. Its like
    a config file that says how the different
    parts are to be organized, and then
    converted into a machine readable score
    to be compiled into audio.

    Some tricky stuff is going on with the
    jsonStr and jsonStrField fields of this
    data type. We get the package from a json
    file, but we also sometimes edit that
    json within this software and save it again.
    jsonStr is the last known valid json, and
    jsonStrField is just whatever string is
    in the json field in the UI. Whenever the
    jsonStrField is updated, we try and decode
    into a package, and if that works we store
    that string, and replace the whole package
    with the value we successfully decoded. If
    decoding didnt work, then we just update
    jsonStrField.

    Besides jsonStr and jsonStrField, beatLength
    is the duration of one beat in the piece of
    music. timingVariance is random variance off
    the exact time the beat is supposed to occur
    that each note should have. This helps sounds
    sound like they are 'real' instead of just
    recordings. timingVariance is a super short
    duration of time, but its nonetheless
    relevant to whether, for example, two
    frequencies are in phase, or out of phase;
    a phenomenon easily detectable by human ears.

    All timing information is stored as an Int
    that represents the number of samples in a
    44,100 hertz audio file, so 0.0226ms

-}
type alias Package =
    { jsonStr : String
    , jsonStrField : String
    , validJson : Bool
    , name : String
    , beatLength : Int
    , timingVariance : Int
    , seed : Seed
    , score : List ( String, Int )
    }



-- DECODER --


decoder : Decoder Package
decoder =
    D.string
        |> D.andThen toDecoder


toDecoder : String -> Decoder Package
toDecoder jsonStr =
    case decode jsonStr of
        Ok package ->
            D.succeed package

        Err err ->
            err
                |> D.errorToString
                |> (++) "package decoder failed -> "
                |> D.fail


decode : String -> Result D.Error Package
decode jsonStr =
    D.decodeString (fromStringDecoder jsonStr) jsonStr


fromStringDecoder : String -> Decoder Package
fromStringDecoder jsonStr =
    D.succeed Package
        |> JDP.hardcoded jsonStr
        |> JDP.hardcoded jsonStr
        |> JDP.hardcoded True
        |> JDP.required "name" D.string
        |> JDP.required "beat-length" D.int
        |> JDP.required "timing-variance" D.int
        |> JDP.required "seed" (D.map Random.initialSeed D.int)
        |> JDP.required "score" partsListDecoder


partsListDecoder : Decoder (List ( String, Int ))
partsListDecoder =
    D.map2
        Tuple.pair
        (D.field "name" D.string)
        (D.field "length" D.int)
        |> D.list



-- HELPERS --


setJsonStrField : String -> Package -> Package
setJsonStrField str package =
    case decode str of
        Ok newPackage ->
            newPackage

        Err _ ->
            { package
                | jsonStrField = str
                , validJson = False
            }


saveToDisk : Package -> Cmd msg
saveToDisk { jsonStr } =
    jsonStr
        |> Ports.SavePackageToDisk
        |> Ports.send


saveScoreToDisk : Package -> Array Part -> Maybe (Cmd msg)
saveScoreToDisk package parts =
    buildScore package parts
        |> Maybe.map (scoreToString package)
        |> Maybe.map (Ports.SaveScoreToDisk >> Ports.send)


buildScore : Package -> Array Part -> Maybe (List Beat)
buildScore package parts =
    package.score
        |> List.map (cropPart (Part.toDict parts))
        |> Util.allValues
        |> Maybe.map List.concat


scoreToString : Package -> List Beat -> String
scoreToString package beats =
    beats
        |> List.indexedMap (withBeatTime package)
        |> List.foldr
            (randomizeTiming package.timingVariance)
            ( package.seed, [] )
        |> Tuple.second
        |> List.map Beat.toString
        |> String.join "\n"


randomizeTiming : Int -> ( Int, Beat ) -> ( Seed, List Beat ) -> ( Seed, List Beat )
randomizeTiming variance ( time, beat ) ( seed, beats ) =
    beat
        |> Beat.toList
        |> List.map (Tuple.pair time)
        |> List.foldr (randomizeNoteTiming variance) ( seed, [] )
        |> Tuple.mapSecond (List.map Note.encodeTime)
        |> Tuple.mapSecond Beat.fromList
        |> Tuple.mapSecond (Util.unshift beats)


randomizeNoteTiming : Int -> ( Int, Note ) -> ( Seed, List ( Int, Note ) ) -> ( Seed, List ( Int, Note ) )
randomizeNoteTiming variance ( time, note ) ( seed, notes ) =
    let
        ( timingOffset, newSeed ) =
            Random.step (Random.int -variance variance) seed
    in
    ( newSeed
    , ( timingOffset + time, note ) :: notes
    )


withBeatTime : Package -> Int -> Beat -> ( Int, Beat )
withBeatTime package index beat =
    ( index * package.beatLength, beat )


cropPart : Dict String (Array Beat) -> ( String, Int ) -> Maybe (List Beat)
cropPart parts ( name, length ) =
    parts
        |> Dict.get name
        |> Maybe.map
            (Array.toList << Array.slice 0 length)
