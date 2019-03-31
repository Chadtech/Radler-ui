module Tests
    ( main
    ) where


import Flow


import Test.Hspec (hspec, SpecWith)
import qualified MonoTests as Mono


main :: IO ()
main = 
    hspec tests 


tests :: SpecWith ()
tests =
    Mono.tests