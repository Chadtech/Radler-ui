module Tests
    ( main
    ) where


import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Mono

main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            (Mono.singleton 0.5) `shouldBe` (Mono.singleton 0.5)