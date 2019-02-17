module Tests exposing (tests)

import Data.Package as Package
import Data.Part as Part
import Expect exposing (Expectation)
import Test exposing (Test, describe)


tests : Test
tests =
    describe "Radler Tests"
        [ Package.tests
        , Part.tests
        ]
