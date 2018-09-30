{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Main (main) where

import Data.Response (Response)
import qualified Data.Response as Response
import qualified Data.Route as Route
import qualified Data.String as String
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Flow
import Model (Model)
import qualified Model
import Msg
import Network.Wai.Middleware.RequestLogger as NWMR
import Program (Program)
import qualified Program
import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Web
import Update (update)
import qualified Control.Monad.Reader as CMR
import qualified Control.Concurrent.STM as STM
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS
import Prelude.Extra (debugLog)
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.IO.Class (liftIO)

-- MAIN -- 


main :: IO ()
main = 
    Program.init Model.init router


-- ROUTER --


router :: ScottyT Text Program ()
router =
    Web.middleware NWMR.logStdoutDev
        >> Web.get "/" sendTestText
        >> Web.get "/plusone" (respond ChangeTestTxt)
        >> get "/ping"
        -- >> (Web.post "/count" $ do
        --     wb <- Web.body -- this must happen before first 'rd'
        --     rd <- Web.bodyReader
        --     chunk <- rd
        --     let body = show chunk
        --     Web.text $ LT.pack $ "uploaded " ++ show len ++ " bytes, wb len is " ++ show (BSL.length wb))

get :: Text -> ScottyT Text Program ()
get routeTxt =
    Web.get 
        (Web.capture (LT.unpack routeTxt)) 
        (withBody routeTxt) 


post :: Text -> ScottyT Text Program ()
post routeTxt =
    Web.post 
        (Web.capture (LT.unpack routeTxt))  
        (withBody routeTxt) 
    
    
withBody :: Text -> Response
withBody routeTxt = do
    wb <- Web.body
    rd <- Web.bodyReader
    chunk <- rd
    let body = T.pack (show chunk)
    respond 
        (Request (Route.decode (T.pack (LT.unpack routeTxt)) body))

respond :: Msg -> Response
respond msg =
    Program.model
        >>= fromModel msg


fromModel :: Msg -> Model -> Response
fromModel msg model =
    let 
        (newModel, response) = 
            update msg model
    in
    Program.setModel newModel
        >> response



sendTestText :: Web.ActionT Text Program ()
sendTestText =
    Program.model
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
        

