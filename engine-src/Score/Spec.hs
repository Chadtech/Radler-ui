{-# LANGUAGE OverloadedStrings #-}


module Score.Spec
    ( tests
    ) where


import Flow

import qualified Audio
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
    \0,1,20;1,2,30\n3,3,21;4,4,30\n\
    \:\n\
    \#config\n\
    \major 7 tone jit;1;0;x=5y=3z=7width=10length=12height=17"


tests :: SpecWith ()
tests =
    case Score.fromText testText of
        Right score ->
            Test.describe "score tests" <| do
                Test.specify "Score builds" <| do
                    Score.toDevAudio score
                        |> expect
                            (Audio.fromList [10.0,15.0,0.0,10.5,15.0])

        Left error ->
            Test.specify (show error) <| do
                expect True False


