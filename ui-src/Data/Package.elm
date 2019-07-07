module Data.Package exposing
    ( Package
    , ScoreParams
    , decoder
    , saveToDisk
    , scorePayload
    , setJsonStrField
    , tests
    )

import Array exposing (Array)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Note as Note exposing (Note)
import Data.Part as Part exposing (Part)
import Data.Room as Room exposing (Room)
import Dict exposing (Dict)
import Expect
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Ports
import Random exposing (Generator, Seed)
import Test exposing (Test, describe, test)
import Util.List as ListUtil
import Util.Maybe as MaybeUtil



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
    , room : Maybe Room
    , scale : String
    }



-- DECODER --


decoder : Decoder Package
decoder =
    let
        fromString : String -> Decoder Package
        fromString jsonStr =
            case decode jsonStr of
                Ok package ->
                    Decode.succeed package

                Err err ->
                    err
                        |> Decode.errorToString
                        |> (++) "package decoder failed -> "
                        |> Decode.fail
    in
    Decode.string
        |> Decode.andThen fromString


decode : String -> Result Decode.Error Package
decode jsonStr =
    Decode.decodeString (fromStringDecoder jsonStr) jsonStr


fromStringDecoder : String -> Decoder Package
fromStringDecoder jsonStr =
    Decode.succeed Package
        |> Pipe.hardcoded jsonStr
        |> Pipe.hardcoded jsonStr
        |> Pipe.hardcoded True
        |> Pipe.required "name" Decode.string
        |> Pipe.required "beat-length" Decode.int
        |> Pipe.required "timing-variance" Decode.int
        |> Pipe.required "seed" (Decode.map Random.initialSeed Decode.int)
        |> Pipe.required "score" partsListDecoder
        |> Pipe.required "voices" (Decode.list Decode.string)
        |> Pipe.optional "room" (Decode.map Just Room.decoder) Nothing
        |> Pipe.required "scale" Decode.string


partsListDecoder : Decoder (List ( String, Int ))
partsListDecoder =
    Decode.map2
        Tuple.pair
        (Decode.field "name" Decode.string)
        (Decode.field "length" Decode.int)
        |> Decode.list



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

    -- Maybe a length, if there is
    -- no length then it means build
    -- the score all the way to the end
    , length : Maybe Int
    }


scorePayload : ScoreParams -> Maybe String
scorePayload params =
    buildScore params
        |> Maybe.map
            (toScoreString params.package)


scorePayloadTests : Package -> Test
scorePayloadTests package =
    describe "Score Payload"
        [ test "building the score payload without any parts will not work" <|
            \_ ->
                { package = package
                , parts = Array.fromList []
                , from = 0
                , length = Nothing
                }
                    |> scorePayload
                    |> Expect.equal Nothing
        , test "building the score payload works as expected" <|
            \_ ->
                { package = package
                , parts =
                    Array.fromList
                        [ Part.empty "part-a"
                        , Part.empty "part-b"
                        ]
                , from = 0
                , length = Nothing
                }
                    |> scorePayload
                    |> Expect.equal (Just "# NAME\ntest-song\n:\n# VOICES\nsaw | position(x=-5 y=1 z=1) freqerror(0.01);sin | position(x=-2 y=3 z=1) freqerror(0.01)\n:\n# NOTES\n29,330387,X;68,388166,X;24,521196,X;-25,75633,X;-3,3565,X;-80,349823,X\n4974,420885,X;4914,177705,X;5028,295036,X;5099,8121,X;4948,372517,X;5071,304552,X\n9976,134539,X;10057,386314,X;10055,290476,X;10075,113961,X;9945,433682,X;9903,203817,X\n15044,312370,X;15047,156784,X;14939,424180,X;15054,271311,X;15008,499779,X;15021,349071,X\n:\n# CONFIG\nmajor 7 tone jit;5000;100;x=5y=3z=7width=10length=12height=17")
        ]


toScoreString : Package -> List (Beat Encoding.None) -> String
toScoreString package score =
    let
        withBeatTime : Int -> Beat Encoding.None -> ( Int, Beat Encoding.None )
        withBeatTime index beat =
            ( index * package.beatLength, beat )

        randomOffsetAndSeed : Generator ( Int, Int )
        randomOffsetAndSeed =
            Random.map2
                Tuple.pair
                (Random.int -package.timingVariance package.timingVariance)
                (Random.int 0 524288)

        randomizeNoteTiming :
            ( Int, Note Encoding.None )
            -> ( Seed, List (Note Encoding.Backend) )
            -> ( Seed, List (Note Encoding.Backend) )
        randomizeNoteTiming ( time, note ) ( seed, encodedNotes ) =
            let
                ( ( timingOffset, noteSeed ), newSeed ) =
                    Random.step randomOffsetAndSeed seed
            in
            ( newSeed
            , Note.encode (time + timingOffset) noteSeed note :: encodedNotes
            )

        randomizeTiming :
            ( Int, Beat Encoding.None )
            -> ( Seed, List (Beat Encoding.Backend) )
            -> ( Seed, List (Beat Encoding.Backend) )
        randomizeTiming ( time, beat ) ( seed, beats ) =
            beat
                |> Beat.toList
                |> List.map (Tuple.pair time)
                |> List.foldr randomizeNoteTiming ( seed, [] )
                |> Tuple.mapSecond
                    (Beat.fromList >> ListUtil.unshift beats)
    in
    [ "# NAME"
    , package.name
    , ":"
    , "# VOICES"
    , String.join ";" package.voices
    , ":"
    , "# NOTES"
    , score
        |> List.indexedMap withBeatTime
        |> List.foldl randomizeTiming ( package.seed, [] )
        |> Tuple.second
        |> List.reverse
        |> List.map Beat.toString
        |> String.join "\n"
    , ":"
    , "# CONFIG"
    , String.join ";"
        [ package.scale
        , String.fromInt package.beatLength
        , String.fromInt package.timingVariance
        , package.room
            |> Maybe.map Room.toString
            |> Maybe.withDefault "no-room"
        ]
    ]
        |> String.join "\n"


buildScore : ScoreParams -> Maybe (List (Beat Encoding.None))
buildScore params =
    let
        parts : Dict String (Array (Beat Encoding.None))
        parts =
            Part.toDict params.parts

        -- In the package json, each part is
        -- list with its length in beats, but the actual
        -- csv of that part may be longer. This is
        -- possibility exists because the csv is
        -- human-facing, and sometimes it makes sense
        -- either in the UI to give yourself more space
        -- or in the project UX to write more music
        -- than is necessary and to crop it down later.
        -- cropPart takes a part, and cuts it down
        -- to the length specified in the package json.
        cropPart : ( String, Int ) -> Maybe (List (Beat Encoding.None))
        cropPart ( name, length ) =
            parts
                |> Dict.get name
                |> Maybe.map
                    (Array.toList << Array.slice 0 length)

        takeBeginningOfScore : List (Beat Encoding.None) -> List (Beat Encoding.None)
        takeBeginningOfScore beats =
            case params.length of
                Just length ->
                    List.take length beats

                Nothing ->
                    beats

        cropScore : List (List (Beat Encoding.None)) -> List (Beat Encoding.None)
        cropScore pieces =
            pieces
                |> List.concat
                |> List.drop params.from
                |> takeBeginningOfScore
    in
    params.package.score
        |> List.map cropPart
        |> MaybeUtil.allValues
        |> Maybe.map cropScore



-- TESTS --


tests : Package -> Test
tests testPackage =
    scorePayloadTests testPackage
