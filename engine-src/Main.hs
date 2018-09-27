{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Main (main) where

import qualified Data.String as String
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Flow
import Model (Model)
import qualified Model
import Network.Wai.Middleware.RequestLogger as NWMR
import Program (Program)
import qualified Program
import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Web
import Update (update)


-- MAIN -- 


main :: IO ()
main = 
    Program.init Model.init router


-- ROUTER --


router :: ScottyT Text Program ()
router =
    Web.middleware NWMR.logStdoutDev 
        >> Web.get "/" sendTestText
        >> Web.get "/plusone" plusoneRoute


plusoneRoute :: Web.ActionT Text Program ()
plusoneRoute =
    Program.set (Program.mapModel (Model.setTestTxt "Yikes"))
        >> Web.redirect "/"


sendTestText :: Web.ActionT Text Program ()
sendTestText =
    Program.set Program.getModel
        >>= showTestText
        

showTestText :: Model -> Web.ActionT Text Program ()
showTestText m =
    m
        |> Model.testTxt
        |> T.unpack 
        |> String.fromString 
        |> Web.text
    -- >> (Web.get "/plusone" $ do
    --         setApp (mapModel addOne)
    --         Web.redirect "/")

    -- >> (Web.get "/plustwo" $ do
    --         setApp (mapModel addTwo)
    --         Web.redirect "/")
        

