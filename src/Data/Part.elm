module Data.Part
    exposing
        ( Part
        , addBeat
        , addVoice
        , decoder
        , empty
        , mapBeat
        , removeBeat
        , removeVoice
        , saveToDisk
        , setName
        , toDict
        , voiceCount
        )

import Array exposing (Array)
import Data.Beat as Beat exposing (Beat)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Ports


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
    , beats : Array Beat
    }


empty : Part
empty =
    { name = "new-part"
    , beats =
        Beat.empty 6
            |> Array.repeat 64
    }



-- DECODER --


decoder : Decoder Part
decoder =
    D.succeed Part
        |> JDP.required "name" D.string
        |> JDP.required "data" (D.map beatsFromString D.string)


beatsFromString : String -> Array Beat
beatsFromString str =
    str
        |> String.split "\n"
        |> List.map Beat.fromString
        |> Array.fromList



-- HELPERS --


toDict : Array Part -> Dict String (Array Beat)
toDict parts =
    parts
        |> Array.toList
        |> List.map toKeyValue
        |> Dict.fromList


toKeyValue : Part -> ( String, Array Beat )
toKeyValue { name, beats } =
    ( name, beats )


addVoice : Int -> Part -> Part
addVoice index part =
    { part
        | beats =
            Array.map (Beat.addNote index) part.beats
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


addBeat : Int -> Part -> Part
addBeat index part =
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


pushEmptyBeat : Int -> Array Beat -> Array Beat
pushEmptyBeat columnNumber =
    Array.push (Beat.empty columnNumber)


mapBeat : Int -> (Beat -> Beat) -> Part -> Part
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
