port module Ports exposing
    ( JsMsg(..)
    , fromJs
    , savePartToDisk
    , saveTerminalToDisk
    , saveTrackersToDisk
    , send
    )

import Data.Index as Index exposing (Index)
import Data.Part as Part exposing (Part)
import Data.Tracker as Tracker exposing (Tracker)
import Json.Encode as Encode exposing (Value)


type JsMsg
    = SavePartToDisk Part
    | SaveTrackersToDisk (List Tracker)
    | SavePackageToDisk String
    | DeletePartFromDisk String (Index Part)
    | SaveTerminalToDisk String


saveTerminalToDisk : String -> Cmd msg
saveTerminalToDisk =
    SaveTerminalToDisk >> send


savePartToDisk : Part -> Cmd msg
savePartToDisk =
    SavePartToDisk >> send


saveTrackersToDisk : List Tracker -> Cmd msg
saveTrackersToDisk =
    SaveTrackersToDisk >> send


toCmd : String -> Value -> Cmd msg
toCmd type_ payload =
    [ field "type" <| Encode.string type_
    , field "payload" <| payload
    ]
        |> Encode.object
        |> toJs


send : JsMsg -> Cmd msg
send msg =
    case msg of
        SavePartToDisk part ->
            part
                |> Part.encode
                |> toCmd "savePartToDisk"

        SavePackageToDisk package ->
            package
                |> Encode.string
                |> toCmd "savePackageToDisk"

        DeletePartFromDisk partName partIndex ->
            [ field "name" <| Encode.string partName
            , field "index" <| Index.encode partIndex
            ]
                |> Encode.object
                |> toCmd "deletePartFromDisk"

        SaveTrackersToDisk trackers ->
            trackers
                |> Encode.list Tracker.encode
                |> toCmd "saveTrackersToDisk"

        SaveTerminalToDisk terminal ->
            terminal
                |> Encode.string
                |> toCmd "saveTerminalToDisk"


field : String -> Encode.Value -> ( String, Encode.Value )
field =
    Tuple.pair


port toJs : Value -> Cmd msg


port fromJs : (Value -> msg) -> Sub msg
