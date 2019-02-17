module Score exposing
    ( HttpPayload
    , errorToString
    , sendHttp
    )

import Http
import Json.Decode as Decode


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "bad url -> " ++ url

        Http.Timeout ->
            "time out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus code ->
            "Bad Status" ++ String.fromInt code

        Http.BadBody decodeError ->
            "Decoder problem -> " ++ decodeError


type alias HttpPayload msg =
    { path : String
    , score : String
    , msgCtor : Result Http.Error () -> msg
    }


sendHttp : HttpPayload msg -> Cmd msg
sendHttp { path, score, msgCtor } =
    Http.post
        { url = path
        , body =
            Http.stringBody "string" score
        , expect =
            Http.expectJson msgCtor <| Decode.null ()
        }
