port module Ports exposing
    ( JsMsg(..)
    , fromJs
    , send
    )

import Json.Encode as Encode exposing (Value)


type JsMsg
    = SavePartToDisk ( String, String )
    | SavePackageToDisk String


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
        SavePartToDisk ( name, data ) ->
            [ field "name" (Encode.string name)
            , field "data" (Encode.string data)
            ]
                |> Encode.object
                |> toCmd "savePartToDisk"

        SavePackageToDisk package ->
            package
                |> Encode.string
                |> toCmd "savePackageToDisk"


field : String -> Encode.Value -> ( String, Encode.Value )
field =
    Tuple.pair


port toJs : Value -> Cmd msg


port fromJs : (Value -> msg) -> Sub msg
