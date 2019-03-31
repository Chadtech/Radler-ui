module MonoTests
    ( tests
    ) where

        
import Flow

import Test.Extra (expect)
import Test.Hspec (SpecWith)
import qualified Test.Hspec as Test
import Freq (Freq(Freq))
import Duration (Duration(Duration))
import Mono (Mono)
import qualified Mono
import Volume (Volume(Volume))


one :: Mono
one =
    Mono.singleton 1


zero :: Mono
zero =
    Mono.singleton 0


onezerozero :: Mono
onezerozero =
    Mono.fromList [ 1, 0, 0 ]


tests :: SpecWith ()
tests = 
    Test.describe "Mono" <| do
        Test.it "concatenates" <| do
            Mono.concat
                [ one
                , zero
                , zero
                ]
                |> expect onezerozero

        Test.it "appends" <| do
            Mono.append one zero
                |> Mono.toList
                |> expect [ 1, 0 ]

        Test.it "can turn into a list of samples" <| do
            [ 1, 0 ]
                |> Mono.fromList
                |> Mono.toSamples 
                |> expect [ Mono.maxInt32Sample, 0 ]

        Test.it "gives the length" <| do
            Mono.length one
                |> expect 1

        Test.describe "TrimEnd" <| do
            Test.it "can trim the end of a mono" <| do
                Mono.append
                    onezerozero
                    onezerozero
                    |> Mono.trimEnd
                    |> expect (Mono.append onezerozero one)

            Test.it "wont trim anything of the end of mono that doesnt end in zero" <| do
                Mono.append
                    onezerozero
                    one
                    |> Mono.trimEnd
                    |> expect (Mono.append onezerozero one)

        Test.it "applies until" <| do
            Mono.applyUntil
                2
                (Mono.map ((*) 2))
                (Mono.fromList [1, 0.5, 1, 1])
                |> Mono.toList
                |> expect [2, 1, 1, 1]

        Test.it "applies from" <| do
            Mono.applyFrom
                2
                (Mono.map ((*) 2))
                (Mono.fromList [1, 0.5, 1, 1])
                |> Mono.toList
                |> expect [1, 0.5, 2, 2]

        Test.it "declips" <| do
            Mono.declip__testable
                2
                (Mono.fromList [1, 1, 1, 1, 1 ])
                |> Mono.toList
                |> expect [0, 0.5, 1, 0.5, 0]

        Test.it "delays" <| do
            Mono.delay (Duration 1) one
                |> Mono.toList
                |> expect [ 0, 1 ]

        Test.it "equalizes lengths" <| do
            Mono.equalizeLengths
                one
                onezerozero
                |> expect
                    (onezerozero, onezerozero)

        Test.it "mixes" <| do
            Mono.mix
                (Mono.fromList [1, 1, 0.5, 0.1, 0, 0.1 ])
                (Mono.fromList [0, -0.1, 0.5, 1, 1, 0.5, 0 ])
                |> Mono.toList
                |> expect [ 1, 0.9, 1, 1.1, 1, 0.6, 0 ]

        Test.it "mixes many" <| do
            Mono.mixMany
                [ Mono.fromList
                    [ 1, 0, 0, 0, 1, 0 ]
                , Mono.fromList
                    [ 0, 0.5, -0.5 ]
                , Mono.fromList
                    [ 0, 0, 0.5, 0.5 ]
                ]
                |> Mono.toList
                |> expect 
                    [ 1, 0.5, 0, 0.5, 1, 0 ]

        Test.it "saw" <| do
            Mono.saw__testable
                4
                (Freq 1)
                (Duration 8)
                |> Mono.toList
                |> expect
                    [ 0, 0.5, -1, -0.5, 0, 0.5, -1, -0.5 ]

        Test.it "sets volume" <| do
            Mono.setVolume
                (Volume 0.5)
                (Mono.fromList [0, 0.5, 1, -0.5, -1 ])
                |> Mono.toList
                |> expect
                    [ 0, 0.25, 0.5, -0.25, -0.5 ]

        Test.it "splits at" <| do
            Mono.splitAt 
                2 
                (Mono.fromList [-1, 0, 1, 2])
                |> expect
                    ( Mono.fromList [ -1, 0 ]
                    , Mono.fromList [ 1, 2 ]
                    )

        Test.it "subtracts" <| do
            Mono.subtract
                (Mono.fromList [1, 1, 0.5, 0.1, 0, 0.1 ])
                (Mono.fromList [0, -0.1, 0.5, 1, 1, 0.5, 0 ])
                |> Mono.toList
                |> expect [ -1, -1.1, 0, 0.9, 1, 0.4, 0 ]

        Test.it "inverts" <| do
            Mono.invert one
                |> Mono.toList
                |> expect [ -1 ]