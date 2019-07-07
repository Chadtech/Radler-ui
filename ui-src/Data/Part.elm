module Data.Part exposing
    ( Part
    , addBeatBelow
    , addBeatToBeginning
    , addVoice
    , decoder
    , empty
    , encode
    , mapBeat
    , removeBeat
    , removeVoice
    , setName
    , tests
    , toDict
    , voiceCount
    )

import Array exposing (Array)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Index as Index exposing (Index)
import Data.Note as Note exposing (Note)
import Dict exposing (Dict)
import Expect
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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


addVoice : Index (Note Encoding.None) -> Part -> Part
addVoice index part =
    { part
        | beats =
            Array.map
                (Beat.addNoteAfter index)
                part.beats
    }


removeVoice : Index (Note Encoding.None) -> Part -> Part
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


removeBeat : Index (Beat Encoding.None) -> Part -> Part
removeBeat index part =
    let
        i : Int
        i =
            Index.toInt index
    in
    { part
        | beats =
            part.beats
                |> Array.slice
                    (i + 1)
                    (Array.length part.beats)
                |> Array.append
                    (Array.slice 0 i part.beats)
    }


addBeatToBeginning : Part -> Part
addBeatToBeginning =
    addBeatBelow (Index.previous Index.zero)


{-| Below as in, below this beat in the UI, so
add a beat into the index one higher than the
index provided.

In the UI, the top most beat is index 0

    *  0
    |  1
    |  2
    v  3

-}
addBeatBelow : Index (Beat Encoding.None) -> Part -> Part
addBeatBelow noteIndex part =
    case Maybe.map Beat.length <| Array.get 0 part.beats of
        Just beatLength ->
            let
                i : Int
                i =
                    Index.toInt <| Index.next noteIndex
            in
            { part
                | beats =
                    part.beats
                        |> Array.slice i (Array.length part.beats)
                        |> Array.append
                            (pushEmptyBeat beatLength (Array.slice 0 i part.beats))
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


mapBeat : Index (Beat Encoding.None) -> (Beat Encoding.None -> Beat Encoding.None) -> Part -> Part
mapBeat index f part =
    let
        i : Int
        i =
            Index.toInt index
    in
    case Array.get i part.beats of
        Just beat ->
            { part
                | beats =
                    Array.set
                        i
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
                |> mapBeat Index.zero (Beat.removeNote Index.zero)
                |> mapBeat (Index.next Index.zero) (Beat.removeNote Index.zero)
                |> .beats
                |> Expect.equal expectedResult


encode : Part -> Encode.Value
encode part =
    [ Tuple.pair "name" <| Encode.string part.name
    , Tuple.pair "data" <| Encode.string <| toString part
    ]
        |> Encode.object


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
