{-# LANGUAGE OverloadedStrings #-}


module Score.Spec
    ( tests
    ) where


import Flow

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Score
import Test.Extra (expect)
import Test.Hspec (SpecWith)
import qualified Test.Hspec as Test


testText :: Text
testText =
    "#name\n\
    \namey\n\
    \:\n\
    \#voices\n\
    \test | position( x=-5 y=1 z=1 );test | position( x=5 y=1 z=1 ) \n\
    \:\n\
    \#notes\n\
    \100,1,20;110,2,30\n200,3,21;190,4,30\n\
    \:\n\
    \#config\n\
    \major 7 tone jit;2000;100;x=5y=3z=7width=10length=12height=17"


tests :: SpecWith ()
tests =
    case Score.fromText testText of
        Right score ->
            Test.specify "Parsed from text" <| do
                expect () ()

        Left error ->
            Test.specify (show error) <| do
                expect True False


