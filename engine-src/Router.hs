{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Router 
    ( Router
    , get
    , post
    ) where

        
import Flow
import Prelude.Extra

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as CMR
import qualified Data.ByteString as BS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Model (Model)
import qualified Model
import Program (Program)
import qualified Program
import Response (Response)
import qualified Response
import Route (Route)
import qualified Route
import Update (update)
import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Web
import Language.Haskell.TH.Ppr (bytesToString)


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
    let 
        step acc = do 
            chunk <- rd
            let len = BS.length chunk
            if len > 0 then 
                chunk
                    |> BS.unpack
                    |> bytesToString
                    |> (++) acc
                    |> step
            else 
                return acc
    bodyTxt <- liftIO <| step ""
    route <- liftIO <| Route.decode routeTxt <| T.pack bodyTxt
    respond route


respond :: Maybe Route -> Response
respond route =
    Program.model
        |> andThen (update route)

        