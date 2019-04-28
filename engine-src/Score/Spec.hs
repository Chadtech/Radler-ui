{-# LANGUAGE OverloadedStrings #-}


module Score.Spec
    ( tests
    ) where


import Flow

import qualified Audio
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Resolution
import qualified Score
import Test.Extra (expect)
import Test.Hspec (SpecWith)
import qualified Test.Hspec as Test


makeTestTextFromNotes :: Text -> Text
makeTestTextFromNotes notes =
    [ "#name\n\
      \namey\n\
      \:\n\
      \#voices\n\
      \test | position( x=-5 y=1 z=1 );test | position( x=5 y=1 z=1 ) \n\
      \:\n\
      \#notes\n"
    , notes
    , ":\n\
      \#config\n\
      \major 7 tone jit;1;0;x=5y=3z=7width=10length=12height=17"
    ]
        |> T.concat


makeNotes :: Text -> Text
makeNotes firstNote =
    [ "0,1,"
    , firstNote
    , ";1,2,30\n3,3,21;4,4,30\n"
    ]
        |> T.concat


testText :: Text
testText =
    makeTestTextFromNotes <| makeNotes "20"


tests :: SpecWith ()
tests =
    case
        "20"
            |> makeNotes
            |> makeTestTextFromNotes
            |> Score.fromText
    of
        Right score ->
            Test.describe "score tests" <| do
                Test.specify "Score builds" <| do
                    Score.toDevAudio score
                        |> expect
                            (Audio.fromList [10.0,15.0,0.0,10.5,15.0])

                Test.specify "Score diffed with itself resolves to identical" <| do
                    Score.diff
                        (Score.Incoming score)
                        (Score.Existing score)
                        |> expect (Right Resolution.Identical)

--                 Test.specify "Score diffed with slightly different version resolves to Changes" <|
--                     case
--                         "21"
--                             |> makeNotes
--                             |> makeTestTextFromNotes
--                             |> Score.fromText
--                     of
--                         Right score2 ->
--                             Score.diff
--                                 (Score.Incoming score2)
--                                 (Score.Existing score)
--                                 |> expect (Right Resolution.Identical)
--
--                         Left error ->
--                             expect True False


        Left error ->
            Test.specify (show error) <| do
                expect True False


