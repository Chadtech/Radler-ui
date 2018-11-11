{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Router 
    ( Router
    , get
    , post
    ) where


import Cmd (Cmd)
import qualified Cmd
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as CMR
import qualified Data.ByteString as BS
import Data.Function
import Data.Response (Response)
import qualified Data.Response as Response
import qualified Data.Route as Route
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Model (Model)
import qualified Model
import Msg
import Program (Program)
import qualified Program
import Update (update)
import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Web


data Router
    = Router (ScottyT Text Program ())


get :: Text -> ScottyT Text Program ()
get routeTxt =
    Web.get 
        (Web.capture (T.unpack routeTxt)) 
        (withBody routeTxt)


post :: Text -> ScottyT Text Program ()
post routeTxt =
    Web.post 
        (Web.capture (T.unpack routeTxt))  
        (withBody routeTxt) 
    
    
withBody :: Text -> Response
withBody routeTxt = do
    wb <- Web.body
    rd <- Web.bodyReader
    let step acc = do 
            chunk <- rd
            let len = BS.length chunk
            if len > 0 then 
                step $ acc ++ (show chunk)
            else 
                return acc
    bodyTxt <- liftIO $ step ""
    bodyTxt
        & T.pack
        & Route.decode routeTxt
        & Request
        & respond


respond :: Msg -> Response
respond msg =
    Program.model
        >>= fromModel msg


fromModel :: Msg -> Model -> Response
fromModel msg model =
    let 
        (newModel, cmd, response) = 
            update msg model
    in
    Program.setModel newModel
        >> Web.liftAndCatchIO (Cmd.toIO cmd)
        >> response
        