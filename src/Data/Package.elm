module Data.Package
    exposing
        ( Package
        , decoder
        , saveToDisk
        , setJsonStrField
        )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Ports
import Random exposing (Seed)


-- TYPES --


type alias Package =
    { jsonStr : String
    , jsonStrField : String
    , validJson : Bool
    , name : String
    , beatLength : Int
    , timingVariance : Int
    , seed : Seed
    }



-- DECODER --


decoder : Decoder Package
decoder =
    D.string
        |> D.andThen toDecoder


toDecoder : String -> Decoder Package
toDecoder jsonStr =
    case decode jsonStr of
        Ok package ->
            D.succeed package

        Err err ->
            err
                |> D.errorToString
                |> (++) "package decoder failed -> "
                |> D.fail


decode : String -> Result D.Error Package
decode jsonStr =
    D.decodeString (fromStringDecoder jsonStr) jsonStr


fromStringDecoder : String -> Decoder Package
fromStringDecoder jsonStr =
    D.succeed Package
        |> JDP.hardcoded jsonStr
        |> JDP.hardcoded jsonStr
        |> JDP.hardcoded True
        |> JDP.required "name" D.string
        |> JDP.required "beat-length" D.int
        |> JDP.required "timing-variance" D.int
        |> JDP.required "seed" (D.map Random.initialSeed D.int)



-- HELPERS --


setJsonStrField : String -> Package -> Package
setJsonStrField str package =
    case decode str of
        Ok newPackage ->
            newPackage

        Err _ ->
            { package
                | jsonStrField = str
                , validJson = False
            }


saveToDisk : Package -> Cmd msg
saveToDisk { jsonStr } =
    jsonStr
        |> Ports.SavePackageToDisk
        |> Ports.send
