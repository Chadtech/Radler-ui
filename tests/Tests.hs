module Tests
    ( main
    ) where


import Flow


import Test.Hspec (hspec, SpecWith)
import qualified Mono.Spec as Mono
import qualified Score.Spec as Score
import qualified Stereo.Spec as Stereo
import qualified Volume.Spec as Volume

main :: IO ()
main = 
    hspec tests 


tests :: SpecWith ()
tests =
    Mono.tests
    >> Stereo.tests
    >> Score.tests
    >> Volume.tests