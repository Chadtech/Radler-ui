{-# LANGUAGE OverloadedStrings #-}


module Data.Response 
    ( Response
    , ping
    ) where


import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Flow
import Program (Program)
import qualified Web.Scotty.Trans as Web


data Response
    = Response (Web.ActionT Text Program ())


ping :: Response
ping =
    "PING"
        |> Web.text
        |> Response