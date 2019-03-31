module Tests
    ( main
    ) where


import Flow


import Test.Hspec (hspec, SpecWith)
import qualified Mono.Spec as Mono
import qualified Stereo.Spec as Stereo


main :: IO ()
main = 
    hspec tests 


tests :: SpecWith ()
tests =
    Mono.tests
    >> Stereo.tests