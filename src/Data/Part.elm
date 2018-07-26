module Data.Part
    exposing
        ( Part
        , addBeat
        , addColumn
        , columnCount
        , decoder
        , empty
        , mapBeat
        , removeBeat
        , removeColumn
        , saveToDisk
        , setName
        )

import Array exposing (Array)
import Data.Beat as Beat
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Ports


-- TYPES --


type alias Part =
    { name : String
    , beats : Array (Array String)
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


beatsFromString : String -> Array (Array String)
beatsFromString str =
    str
        |> String.split "\n"
        |> List.map Beat.fromString
        |> Array.fromList



-- HELPERS --


addColumn : Int -> Part -> Part
addColumn index part =
    { part
        | beats =
            Array.map (addColumnToBeat index) part.beats
    }


addColumnToBeat : Int -> Array String -> Array String
addColumnToBeat index beat =
    beat
        |> Array.slice (index + 1) (Array.length beat)
        |> Array.append
            (Array.push "" (Array.slice 0 (index + 1) beat))


removeColumn : Int -> Part -> Part
removeColumn index part =
    { part
        | beats =
            Array.map (removeColumnFromBeat index) part.beats
    }


removeColumnFromBeat : Int -> Array String -> Array String
removeColumnFromBeat index beat =
    beat
        |> Array.slice (index + 1) (Array.length beat)
        |> Array.append (Array.slice 0 index beat)


setName : String -> Part -> Part
setName str part =
    { part | name = str }


columnCount : Part -> Int
columnCount { beats } =
    beats
        |> Array.get 0
        |> Maybe.map Array.length
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
    case Maybe.map Array.length <| Array.get 0 part.beats of
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


pushEmptyBeat : Int -> Array (Array String) -> Array (Array String)
pushEmptyBeat columnNumber =
    Array.push (Beat.empty columnNumber)


mapBeat : Int -> (Array String -> Array String) -> Part -> Part
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
