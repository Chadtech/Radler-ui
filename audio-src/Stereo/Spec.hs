module Stereo.Spec
  ( tests
  )
where


import           Flow

import           Test.Extra                     ( expect )
import           Test.Hspec                     ( SpecWith )
import qualified Test.Hspec                    as Test
import           Freq                           ( Freq(Freq) )
import           Duration                       ( Duration(Duration) )
import           Mono                           ( Mono )
import qualified Mono
import           Stereo                         ( Stereo )
import qualified Stereo
import           Volume                         ( Volume(Volume) )


one :: Stereo
one = Stereo.singleton (1, 1)


zero :: Stereo
zero = Stereo.singleton (0, 0)


onezerozero :: Stereo
onezerozero = Stereo.fromList [(1, 1), (0, 0), (0, 0)]

tests :: SpecWith ()
tests = Test.describe "Stereo" <| do
  Test.it "apends" <| do
    Stereo.append one zero |> Stereo.toList |> expect [(1, 1), (0, 0)]

  Test.describe "TrimEnd" <| do
    Test.it "can trim the end of a stereo" <| do
      Stereo.append onezerozero onezerozero |> Stereo.trimEnd |> expect
        (Stereo.append onezerozero one)

    Test.it "wont trim anything off the end of a stereo that doesnt end in zero"
      <| do
           Stereo.append onezerozero one |> Stereo.trimEnd |> expect
             (Stereo.append onezerozero one)

  Test.it "mixes" <| do
    Stereo.mix (Stereo.fromList [(0, 0), (1, 0), (-0.5, 0.1)])
               (Stereo.fromList [(0, 1), (-0.5, -0.5)])
      |> Stereo.toList
      |> expect [(0, 1), (0.5, -0.5), (-0.5, 0.1)]
