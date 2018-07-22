module Data.Package
    exposing
        ( Package
        , decoder
        , setJsonStrField
        )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP


-- TYPES --


type alias Package =
    { jsonStr : String
    , jsonStrField : String
    , name : String
    , beatLength : Int
    , timingVariance : Int
    }



-- DECODER --


decoder : String -> Decoder Package
decoder jsonStr =
    case decode jsonStr of
        Ok package ->
            D.succeed package

        Err err ->
            err
                |> D.errorToString
                |> (++) "package decoder failed -> "
                |> D.fail


fromStringDecoder : String -> Decoder Package
fromStringDecoder jsonStr =
    D.succeed Package
        |> JDP.hardcoded jsonStr
        |> JDP.hardcoded jsonStr
        |> JDP.required "name" D.string
        |> JDP.required "beat-length" D.int
        |> JDP.required "timing-variance" D.int


decode : String -> Result D.Error Package
decode jsonStr =
    D.decodeString (fromStringDecoder jsonStr) jsonStr



-- HELPERS --


setJsonStrField : String -> Package -> Package
setJsonStrField str package =
    case decode str of
        Ok newPackage ->
            newPackage

        Err _ ->
            { package | jsonStrField = str }
