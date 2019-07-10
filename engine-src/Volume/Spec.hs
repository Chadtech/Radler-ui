{-# LANGUAGE OverloadedStrings #-}


module Volume.Spec
  ( tests
  )
where


import           Flow

import           Test.Extra                     ( expect )
import           Test.Hspec                     ( SpecWith )
import qualified Test.Hspec                    as Test
import           Volume                         ( Volume(Volume) )
import qualified Volume


tests :: SpecWith ()
tests = Test.describe "Volume" <| do
  Test.specify "Max volume can be read from text" <| do
    Volume.read "ff"
        -- This is deliberately a little less than one
        -- since trying to convert to 1 leads to Int32
        -- overflow problems
                     |> expect (Right <| Volume 0.99609375)

  Test.specify "Half volume can be read from text" <| do
    Volume.read "80" |> expect (Right <| Volume (128 / 256))


  Test.specify "Zero volume can be read from text" <| do
    Volume.read "00" |> expect (Right <| Volume 0)

