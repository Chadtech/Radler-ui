module Score exposing
    ( HttpPayload
    , errorToString
    , sendHttp
    )

import Http
import Json.Decode as D
import Model exposing (Model)


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "bad url -> " ++ url

        Http.Timeout ->
            "time out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus response ->
            [ String.fromInt response.status.code
            , response.status.message
            , response.body
            ]
                |> String.join " - "

        Http.BadPayload decodeError _ ->
            "Decoder problem -> " ++ decodeError


type alias HttpPayload msg =
    { model : Model
    , path : String
    , score : String
    , msgCtor : Result Http.Error () -> msg
    }


sendHttp : HttpPayload msg -> Cmd msg
sendHttp { model, path, score, msgCtor } =
    Http.post
        (Model.urlRoute model path)
        (Http.stringBody "string" score)
        (D.null ())
        |> Http.send msgCtor
