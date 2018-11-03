{-# LANGUAGE OverloadedStrings #-}


module Data.Response 
    ( Response
    , ping
    , text
    , json
    , _404
    ) where


import Data.Function
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Program (Program)
import qualified Web.Scotty.Trans as Web
import Json (Json)
import qualified Json


type Response 
    = Web.ActionT Text Program ()


ping :: Response
ping =
    Web.text "PONG"


text :: Text -> Response
text =
    Web.text


json :: Json -> Response
json 
    = Data.Response.text
    . Json.toText


_404 :: Response
_404 =
    [ ( "code", Json.string "404" ) ]
        & Json.object
        & Data.Response.json