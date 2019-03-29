module Data.Part exposing
    ( Part
    , addBeatBelow
    , addBeatToBeginning
    , addVoice
    , decoder
    , empty
    , mapBeat
    , removeBeat
    , removeVoice
    , saveToDisk
    , setName
    , tests
    , toDict
    , voiceCount
    )

import Array exposing (Array)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Note as Note
import Dict exposing (Dict)
import Expect
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipe
import Ports
import Test exposing (Test, describe, test)



-- TYPES --


{-|

    Part :=
        as in a part of a piece of music
        like what would be called the chorus
        section, or the 'A' in a ABCBA structured
        song.

    Voice :=
        One sound making entity in a song.
        A duet has two voices, for example

        In this software, one voice is one
        column in the spreadsheet.

-}
type alias Part =
    { name : String
    , beats : Array (Beat Encoding.None)
    }


empty : String -> Part
empty name =
    { name = name
    , beats =
        Beat.empty 6
            |> Array.repeat 64
    }



-- DECODER --


decoder : Decoder Part
decoder =
    let
        beatsFromString : String -> Array (Beat Encoding.None)
        beatsFromString str =
            str
                |> String.split "\n"
                |> List.map Beat.fromString
                |> Array.fromList

        beatsDecoder : Decoder (Array (Beat Encoding.None))
        beatsDecoder =
            Decode.map beatsFromString Decode.string
    in
    Decode.map2 Part
        (Decode.field "name" Decode.string)
        (Decode.field "data" beatsDecoder)



-- HELPERS --


toDict : Array Part -> Dict String (Array (Beat Encoding.None))
toDict parts =
    let
        toKeyValue : Part -> ( String, Array (Beat Encoding.None) )
        toKeyValue { name, beats } =
            ( name, beats )
    in
    parts
        |> Array.toList
        |> List.map toKeyValue
        |> Dict.fromList


addVoice : Int -> Part -> Part
addVoice index part =
    { part
        | beats =
            Array.map
                (Beat.addNoteAfter index)
                part.beats
    }


removeVoice : Int -> Part -> Part
removeVoice index part =
    { part
        | beats =
            Array.map
                (Beat.removeNote index)
                part.beats
    }


setName : String -> Part -> Part
setName str part =
    { part | name = str }


voiceCount : Part -> Int
voiceCount { beats } =
    beats
        |> Array.get 0
        |> Maybe.map Beat.length
        |> Maybe.withDefault 0


removeBeat : Int -> Part -> Part
removeBeat index part =
    { part
        | beats =
            part.beats
                |> Array.slice
                    (index + 1)
                    (Array.length part.beats)
                |> Array.append
                    (Array.slice 0 index part.beats)
    }


addBeatToBeginning : Part -> Part
addBeatToBeginning =
    addBeatBelow -1


{-| Below as in, below this beat in the UI, so
add a beat into the index one higher than the
index provided.

In the UI, the top most beat is index 0

    *  0
    |  1
    |  2
    v  3

-}
addBeatBelow : Int -> Part -> Part
addBeatBelow index part =
    case Maybe.map Beat.length <| Array.get 0 part.beats of
        Just beatLength ->
            let
                ni =
                    index + 1
            in
            { part
                | beats =
                    part.beats
                        |> Array.slice ni (Array.length part.beats)
                        |> Array.append
                            (pushEmptyBeat beatLength (Array.slice 0 ni part.beats))
            }

        Nothing ->
            part


addBeatBelowTest : Test
addBeatBelowTest =
    test "Add beat to beginning" <|
        \_ ->
            let
                expectedResult : Array (Beat Encoding.None)
                expectedResult =
                    testBeats
                        |> Array.toList
                        |> (::) (Beat.empty 2)
                        |> Array.fromList
            in
            testPart
                |> addBeatToBeginning
                |> .beats
                |> Expect.equal expectedResult


pushEmptyBeat : Int -> Array (Beat Encoding.None) -> Array (Beat Encoding.None)
pushEmptyBeat columnNumber =
    Array.push (Beat.empty columnNumber)


mapBeat : Int -> (Beat Encoding.None -> Beat Encoding.None) -> Part -> Part
mapBeat index f part =
    case Array.get index part.beats of
        Just beat ->
            { part
                | beats =
                    Array.set
                        index
                        (f beat)
                        part.beats
            }

        Nothing ->
            part


mapBeatTest : Test
mapBeatTest =
    test "mapBeat can remove note" <|
        \_ ->
            let
                expectedResult : Array (Beat Encoding.None)
                expectedResult =
                    [ [ Note.fromString "348080c" ]
                    , [ Note.fromString "358080c" ]
                    ]
                        |> List.map Beat.fromList
                        |> Array.fromList
            in
            testPart
                |> mapBeat 0 (Beat.removeNote 0)
                |> mapBeat 1 (Beat.removeNote 0)
                |> .beats
                |> Expect.equal expectedResult


saveToDisk : Part -> Cmd msg
saveToDisk part =
    part
        |> toFile
        |> Ports.SavePartToDisk
        |> Ports.send


toFile : Part -> ( String, String )
toFile part =
    ( part.name, toString part )


toString : Part -> String
toString part =
    part.beats
        |> Array.map Beat.toString
        |> Array.toList
        |> String.join "\n"


toStringTest : Test
toStringTest =
    test "To String looks right" <|
        \_ ->
            testPart
                |> toString
                |> Expect.equal "QQ;348080c\n334040c;358080c"



-- TESTS --


tests : Test
tests =
    describe "Data.Part"
        [ toStringTest
        , mapBeatTest
        , addBeatBelowTest
        ]


testPart : Part
testPart =
    { name = "part-a"
    , beats = testBeats
    }


testBeats : Array (Beat Encoding.None)
testBeats =
    [ [ Note.fromString "QQ"
      , Note.fromString "348080c"
      ]
    , [ Note.fromString "334040c"
      , Note.fromString "358080c"
      ]
    ]
        |> List.map Beat.fromList
        |> Array.fromList
