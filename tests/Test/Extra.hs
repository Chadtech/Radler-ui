module Test.Extra 
    ( expect
    ) where


import Test.Hspec 
    ( shouldBe
    , Expectation
    , HasCallStack
    )
        
expect :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
expect x y =
    shouldBe y x
        
        