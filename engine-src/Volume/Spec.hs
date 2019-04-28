{-# LANGUAGE OverloadedStrings #-}


module Volume.Spec
    ( tests
    ) where


import Flow

import Test.Extra (expect)
import Test.Hspec (SpecWith)
import qualified Test.Hspec as Test
import Volume (Volume(Volume))
import qualified Volume


tests :: SpecWith ()
tests =
    Test.describe "Volume" <| do
        Test.specify "Max volume can be read from text" <| do
            Volume.read "ff"
                |> expect (Right <| Volume 1)

        Test.specify "Half volume can be read from text" <| do
            Volume.read "80"
                |> expect (Right <| Volume (128 / 255))


        Test.specify "Half volume can be read from text" <| do
            Volume.read "00"
                |> expect (Right <| Volume 0)

