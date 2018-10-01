module Data.Package exposing
    ( Package
    , ScoreParams
    , decoder
    , saveToDisk
    , scorePayload
    , setJsonStrField
    )

import Array exposing (Array)
import Data.Beat as Beat exposing (Beat)
import Data.Note as Note exposing (Note)
import Data.Part as Part exposing (Part)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Ports
import Random exposing (Generator, Seed)
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
    , voices : List String
    }



-- DECODER --


decoder : Decoder Package
decoder =
    JD.string
        |> JD.andThen toDecoder


toDecoder : String -> Decoder Package
toDecoder jsonStr =
    case decode jsonStr of
        Ok package ->
            JD.succeed package

        Err err ->
            err
                |> JD.errorToString
                |> (++) "package decoder failed -> "
                |> JD.fail


decode : String -> Result JD.Error Package
decode jsonStr =
    JD.decodeString (fromStringDecoder jsonStr) jsonStr


fromStringDecoder : String -> Decoder Package
fromStringDecoder jsonStr =
    JD.succeed Package
        |> JDP.hardcoded jsonStr
        |> JDP.hardcoded jsonStr
        |> JDP.hardcoded True
        |> JDP.required "name" JD.string
        |> JDP.required "beat-length" JD.int
        |> JDP.required "timing-variance" JD.int
        |> JDP.required "seed" (JD.map Random.initialSeed JD.int)
        |> JDP.required "score" partsListDecoder
        |> JDP.required "voices" (JD.list JD.string)


partsListDecoder : Decoder (List ( String, Int ))
partsListDecoder =
    JD.map2
        Tuple.pair
        (JD.field "name" JD.string)
        (JD.field "length" JD.int)
        |> JD.list



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
saveToDisk package =
    package.jsonStr
        |> Ports.SavePackageToDisk
        |> Ports.send


type alias ScoreParams =
    { package : Package
    , parts : Array Part
    , from : Int
    , length : Int
    }


scorePayload : ScoreParams -> Maybe String
scorePayload params =
    buildScore params
        |> Maybe.map
            (toScoreString params.package)


toScoreString : Package -> List Beat -> String
toScoreString package score =
    [ "# READ ME"
    , readme
    , "# NAME"
    , package.name
    , ":"
    , "# VOICES"
    , String.join ";" package.voices
    , ":"
    , "# NOTES"
    , scoreToString package score
    ]
        |> String.join "\n"


buildScore : ScoreParams -> Maybe (List Beat)
buildScore { package, parts, from, length } =
    package.score
        |> List.map (cropPart (Part.toDict parts))
        |> Util.allValues
        |> Maybe.map (cropScore from length)


cropScore : Int -> Int -> List (List Beat) -> List Beat
cropScore from length pieces =
    pieces
        |> List.concat
        |> List.drop from
        |> List.take length


scoreToString : Package -> List Beat -> String
scoreToString package beats =
    beats
        |> List.indexedMap (withBeatTime package)
        |> List.foldl
            (randomizeTiming package.timingVariance)
            ( package.seed, [] )
        |> Tuple.second
        |> List.reverse
        |> List.map Beat.toString
        |> String.join "\n"


randomizeTiming : Int -> ( Int, Beat ) -> ( Seed, List Beat ) -> ( Seed, List Beat )
randomizeTiming variance ( time, beat ) ( seed, beats ) =
    beat
        |> Beat.toList
        |> List.map (Tuple.pair time)
        |> List.foldr (randomizeNoteTiming variance) ( seed, [] )
        |> Tuple.mapSecond Beat.fromList
        |> Tuple.mapSecond (Util.unshift beats)


randomizeNoteTiming : Int -> ( Int, Note ) -> ( Seed, List Note ) -> ( Seed, List Note )
randomizeNoteTiming variance ( time, note ) ( seed, notes ) =
    let
        ( ( timingOffset, noteSeed ), newSeed ) =
            Random.step (randomOffsetAndSeed variance) seed
    in
    ( newSeed
    , Note.encode (time + timingOffset) noteSeed note :: notes
    )


randomOffsetAndSeed : Int -> Generator ( Int, Int )
randomOffsetAndSeed variance =
    Random.map2
        Tuple.pair
        (Random.int -variance variance)
        (Random.int 0 524288)


withBeatTime : Package -> Int -> Beat -> ( Int, Beat )
withBeatTime package index beat =
    ( index * package.beatLength, beat )


{-| In the package json, each part is
list with its length in beats, but the actual
csv of that part may be longer. This is
possibility exists because the csv is
human-facing, and sometimes it makes sense
either in the UI to give yourself more space
or in the project UX to write more music
than is necessary and to crop it down later.

cropPart takes a part, and cuts it down
to the length specified in the package json.

-}
cropPart : Dict String (Array Beat) -> ( String, Int ) -> Maybe (List Beat)
cropPart parts ( name, length ) =
    parts
        |> Dict.get name
        |> Maybe.map
            (Array.toList << Array.slice 0 length)


readme : String
readme =
    """#
# The Haskell side of this project separates
# this file by the ":" character. 
#
# It reads each block separately, in different ways, 
# and expects the blocks to be in a certain order.
#
# The content it expects is..
# 0 Read me (which it ignores)
# 1 The name, which it just pulls in as a string
# 2 The voices, which is just a list of voice names 
# separated by commas.
# 3 The actual notes of the score, which is reads as 
# rows and columns. Each cell is the following 
# information separated by commas
#   3.0 The time the note occurs
#   3.1 A seed of randomness, so that random variations
#   in timing, or whatever else, can be generated. Note
#   these seeds are deterministic, and are the same 
#   every time the score is compiled. Its random 
#   relative to everything else about the note, but not 
#   random relative to its initial conditions (a master
#   seed for the whole project and its position in the rows 
#   and columns)
#   3.2 The note content, which usually includes information
#   like tone, volume, duration, and decay.
#
# Also, it ignores content following "#" characters.
#"""
