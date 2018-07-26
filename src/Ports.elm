port module Ports
    exposing
        ( JsMsg(..)
        , fromJs
        , send
        )

import Json.Encode as E exposing (Value)


type JsMsg
    = SavePartToDisk ( String, String )
    | SavePackageToDisk String


toCmd : String -> Value -> Cmd msg
toCmd type_ payload =
    [ ( "type", E.string type_ )
    , ( "payload", payload )
    ]
        |> E.object
        |> toJs


noPayload : String -> Cmd msg
noPayload type_ =
    toCmd type_ E.null


send : JsMsg -> Cmd msg
send msg =
    case msg of
        SavePartToDisk ( name, data ) ->
            [ Tuple.pair "name" (E.string name)
            , Tuple.pair "data" (E.string data)
            ]
                |> E.object
                |> toCmd "savePartToDisk"

        SavePackageToDisk package ->
            package
                |> E.string
                |> toCmd "savePackageToDisk"


port toJs : Value -> Cmd msg


port fromJs : (Value -> msg) -> Sub msg
