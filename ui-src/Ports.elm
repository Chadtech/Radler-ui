port module Ports exposing
    ( JsMsg(..)
    , fromJs
    , send
    )

import Json.Encode as E exposing (Value)


type JsMsg
    = SavePartToDisk ( String, String )
    | SavePackageToDisk String
    | SaveScoreToDisk String
    | Play Int Int


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
            [ def "name" (E.string name)
            , def "data" (E.string data)
            ]
                |> E.object
                |> toCmd "savePartToDisk"

        SavePackageToDisk package ->
            package
                |> E.string
                |> toCmd "savePackageToDisk"

        SaveScoreToDisk score ->
            score
                |> E.string
                |> toCmd "saveScoreToDisk"

        Play from for ->
            [ def "from" (E.int from)
            , def "for" (E.int for)
            ]
                |> E.object
                |> toCmd "play"


def : String -> E.Value -> ( String, E.Value )
def =
    Tuple.pair


port toJs : Value -> Cmd msg


port fromJs : (Value -> msg) -> Sub msg
