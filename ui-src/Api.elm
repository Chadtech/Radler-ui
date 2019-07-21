module Api exposing
    ( Endpoint
    , Endpoints
    , Error
    , PostParams
    , errorToString
    , fromPortNumber
    , post
    )

import Http
import Json.Decode as Decode



-- TYPES --


type Endpoint
    = Endpoint String


type alias Endpoints =
    { play : Endpoint
    , build : Endpoint
    , terminal : Endpoint
    }


type Error
    = ExpectedError String
    | Other Http.Error



-- PUBLIC HELPERS --


fromPortNumber : Int -> Endpoints
fromPortNumber portNumber =
    let
        onMountPath : String -> Endpoint
        onMountPath route =
            [ "http://localhost:"
            , String.fromInt portNumber
            , "/"
            , route
            ]
                |> String.concat
                |> Endpoint
    in
    { play = onMountPath "play"
    , build = onMountPath "build"
    , terminal = onMountPath "terminal"
    }


type alias PostParams msg =
    { endpoint : Endpoint
    , body : String
    , expect : Result Error () -> msg
    }


post : PostParams msg -> Cmd msg
post { endpoint, body, expect } =
    Http.post
        { url = endpointToString endpoint
        , body = Http.stringBody "string" body
        , expect =
            Http.expectStringResponse
                expect
                responseToError
        }


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



-- PRIVATE HELPERS --


endpointToString : Endpoint -> String
endpointToString (Endpoint str) =
    str


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
                toApiError error =
                    error
                        |> Decode.errorToString
                        |> Http.BadBody
                        |> Other
            in
            body
                |> Decode.decodeString (Decode.null ())
                |> Result.mapError toApiError
