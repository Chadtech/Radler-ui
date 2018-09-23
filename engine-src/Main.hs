{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Main (main) where

import Data.Text.Lazy (Text)
import Network.Wai.Middleware.RequestLogger as NWMR
import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Web
-- import Flow
import Model (Model)
import qualified Model
import Program (Program)
import qualified Program


-- MAIN -- 


main :: IO ()
main = 
    Program.init Model.init router


-- ROUTER --


router :: ScottyT Text Program ()
router =
    Web.middleware NWMR.logStdoutDev 
    >> Web.get "/" (Web.text "ping!!!!")
    -- >> (Web.get "/plusone" $ do
    --         setApp (mapModel addOne)
    --         Web.redirect "/")

    -- >> (Web.get "/plustwo" $ do
    --         setApp (mapModel addTwo)
    --         Web.redirect "/")
        


    