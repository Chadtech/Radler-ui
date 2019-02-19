module Tests exposing (tests)

import Array exposing (Array)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Flags as Flags exposing (Flags)
import Data.Note
import Data.Package
import Data.Part exposing (Part)
import Expect exposing (Expectation)
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (Model)
import Test exposing (Test, describe, test)
import Ui.Modal.Build


tests : Test
tests =
    case
        Decode.decodeValue
            Data.Package.decoder
            (Encode.string testPackageJson)
    of
        Ok package ->
            let
                testModel : Model
                testModel =
                    { package = package
                    , parts =
                        [ testPart "part-a"
                        , testPart "part-b"
                        ]
                            |> Array.fromList
                    , enginePort = 3000
                    }
                        |> Model.init
            in
            describe "Radler Tests"
                [ Data.Package.tests package
                , Data.Part.tests
                , Data.Note.tests
                , Ui.Modal.Build.tests testModel
                ]

        Err err ->
            test "Test package failed to decode" <|
                \_ ->
                    Expect.fail (Decode.errorToString err)


testPart : String -> Part
testPart name =
    { name = name
    , beats = testBeats
    }


testBeats : Array (Beat Encoding.None)
testBeats =
    [ [ Data.Note.fromString "QQ"
      , Data.Note.fromString "348080c"
      ]
    , [ Data.Note.fromString "334040c"
      , Data.Note.fromString "358080c"
      ]
    ]
        |> List.map Beat.fromList
        |> Array.fromList


testPackageJson : String
testPackageJson =
    """{
    "name": "test-song",
    "parts-src": "./parts",
    "score": [
        {
            "name": "part-a",
            "length": 2
        },
        {
            "name": "part-b",
            "length": 2
        }
    ],
    "voices": [
        "saw | position(x=-5 y=1 z=1) freqerror(0.01)",
        "sin | position(x=-2 y=3 z=1) freqerror(0.01)"
    ],
    "room": {
         "size": {
              "width": 10,
              "length": 12,
              "height": 17
          },
          "listener-position": {
              "x": 5,
              "y": 3,
              "z": 7 
          }
    },
    "seed": 19,
    "timing-variance": 100,
    "beat-length": 5000,
    "scale": "major 7 tone jit"
}"""
