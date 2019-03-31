module MonoTests
    ( tests
    ) where

import Flow


import Test.Hspec 
    ( describe
    , it
    , shouldBe
    , SpecWith
    , Expectation
    , HasCallStack
    )
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


expect :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
expect x y =
    shouldBe y x


tests :: SpecWith ()
tests = 
    describe "Mono" <| do
        it "concatenates" <| do
            Mono.concat
                [ one
                , zero
                , zero
                ]
                |> expect onezerozero

        it "appends" <| do
            Mono.append one zero
                |> Mono.toList
                |> expect [ 1, 0 ]

        it "can turn into a list of samples" <| do
            [ 1, 0 ]
                |> Mono.fromList
                |> Mono.toSamples 
                |> expect [ Mono.maxInt32Sample, 0 ]

        it "gives the length" <| do
            Mono.length one
                |> expect 1

        describe "TrimEnd" <| do
            it "can trim the end of a mono" <| do
                Mono.append
                    onezerozero
                    onezerozero
                    |> Mono.trimEnd
                    |> expect (Mono.append onezerozero one)

            it "wont trim anything of the end of mono that doesnt end in zero" <| do
                Mono.append
                    onezerozero
                    one
                    |> Mono.trimEnd
                    |> expect (Mono.append onezerozero one)

        it "applies until" <| do
            Mono.applyUntil
                2
                (Mono.map ((*) 2))
                (Mono.fromList [1, 0.5, 1, 1])
                |> Mono.toList
                |> expect [2, 1, 1, 1]

        it "applies from" <| do
            Mono.applyFrom
                2
                (Mono.map ((*) 2))
                (Mono.fromList [1, 0.5, 1, 1])
                |> Mono.toList
                |> expect [1, 0.5, 2, 2]

        it "declips" <| do
            Mono.declip__testable
                2
                (Mono.fromList [1, 1, 1, 1, 1 ])
                |> Mono.toList
                |> expect [0, 0.5, 1, 0.5, 0]

        it "delays" <| do
            Mono.delay (Duration 1) one
                |> Mono.toList
                |> expect [ 0, 1 ]

        it "equalizes lengths" <| do
            Mono.equalizeLengths
                one
                onezerozero
                |> expect
                    (onezerozero, onezerozero)

        it "mixes" <| do
            Mono.mix
                (Mono.fromList [1, 1, 0.5, 0.1, 0, 0.1 ])
                (Mono.fromList [0, -0.1, 0.5, 1, 1, 0.5, 0 ])
                |> Mono.toList
                |> expect [ 1, 0.9, 1, 1.1, 1, 0.6, 0 ]

        it "mixes many" <| do
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

        it "saw" <| do
            Mono.saw__testable
                4
                (Freq 1)
                (Duration 8)
                |> Mono.toList
                |> expect
                    [ 0, 0.5, -1, -0.5, 0, 0.5, -1, -0.5 ]

        it "sets volume" <| do
            Mono.setVolume
                (Volume 0.5)
                (Mono.fromList [0, 0.5, 1, -0.5, -1 ])
                |> Mono.toList
                |> expect
                    [ 0, 0.25, 0.5, -0.25, -0.5 ]

        it "splits at" <| do
            Mono.splitAt 
                2 
                (Mono.fromList [-1, 0, 1, 2])
                |> expect
                    ( Mono.fromList [ -1, 0 ]
                    , Mono.fromList [ 1, 2 ]
                    )

        it "subtracts" <| do
            Mono.subtract
                (Mono.fromList [1, 1, 0.5, 0.1, 0, 0.1 ])
                (Mono.fromList [0, -0.1, 0.5, 1, 1, 0.5, 0 ])
                |> Mono.toList
                |> expect [ -1, -1.1, 0, 0.9, 1, 0.4, 0 ]

        it "inverts" <| do
            Mono.invert one
                |> Mono.toList
                |> expect [ -1 ]