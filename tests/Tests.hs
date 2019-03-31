module Tests
    ( main
    ) where


import Flow


import Test.Hspec (hspec, SpecWith)
import qualified Mono.Spec as Mono


main :: IO ()
main = 
    hspec tests 


tests :: SpecWith ()
tests =
    Mono.tests