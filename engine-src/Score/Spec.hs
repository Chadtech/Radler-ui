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
    \asdf\n\
    \:\n\
    \#notes\n\
    \asdfdsa\n\
    \:\n\
    \#config\n\
    \1afkjasdfaksd"


tests :: SpecWith ()
tests =
    case Score.fromText testText of
        Right score ->
            Test.specify "Parsed from text" <| do
                expect () ()

        Left error ->
            Test.specify (show error) <| do
                expect True False


