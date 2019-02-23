module Api exposing
    ( Endpoint
    , Endpoints
    , Error
    , endpointsFromPortNumberDecoder
    , errorToString
    , sendScore
    )

import Http
import Json.Decode as Decode exposing (Decoder)



-- TYPES --


type Endpoint
    = Endpoint String


type alias Endpoints =
    { play : Endpoint
    , build : Endpoint
    }


type alias ScorePayload msg =
    { endpoint : Endpoint
    , score : String
    , msgCtor : Result Error () -> msg
    }


type Error
    = ExpectedError String
    | Other Http.Error



-- HELPERS --


endpointsFromPortNumberDecoder : Decoder Endpoints
endpointsFromPortNumberDecoder =
    let
        fromPortNumber : Int -> Decoder Endpoints
        fromPortNumber portNumber =
            let
                onMountPath : String -> String
                onMountPath route =
                    [ "http://localhost:"
                    , String.fromInt portNumber
                    , "/"
                    , route
                    ]
                        |> String.concat
            in
            { play = Endpoint <| onMountPath "play"
            , build = Endpoint <| onMountPath "build"
            }
                |> Decode.succeed
    in
    Decode.int
        |> Decode.andThen fromPortNumber


endpointToString : Endpoint -> String
endpointToString (Endpoint str) =
    str


sendScore : ScorePayload msg -> Cmd msg
sendScore { endpoint, score, msgCtor } =
    Http.post
        { url = endpointToString endpoint
        , body = Http.stringBody "string" score
        , expect =
            Http.expectStringResponse
                msgCtor
                responseToError
        }


responseToError : Http.Response String -> Result Error ()
responseToError response =
    case response of
        Http.BadUrl_ url ->
            Err <| Other <| Http.BadUrl url

        Http.Timeout_ ->
            Err <| Other Http.Timeout

        Http.NetworkError_ ->
            Err <| Other Http.NetworkError

        Http.BadStatus_ metadata _ ->
            case metadata.statusCode of
                400 ->
                    metadata.statusText
                        |> ExpectedError
                        |> Err

                _ ->
                    metadata.statusCode
                        |> Http.BadStatus
                        |> Other
                        |> Err

        Http.GoodStatus_ _ body ->
            let
                toApiError : Decode.Error -> Error
                toApiError =
                    Decode.errorToString
                        >> Http.BadBody
                        >> Other
            in
            body
                |> Decode.decodeString (Decode.null ())
                |> Result.mapError toApiError


errorToString : Error -> String
errorToString error =
    case error of
        ExpectedError str ->
            str

        Other httpError ->
            case httpError of
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
