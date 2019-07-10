{-# LANGUAGE OverloadedStrings #-}


module Response
  ( Response
  , ping
  , text
  , json
  , Response.error
  )
where


import           Data.Function
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as TE
import           Program                        ( Program )
import qualified Web.Scotty.Trans              as Web
import           Json                           ( Json )
import qualified Json
import qualified Network.HTTP.Types.Status     as Status
import qualified Data.ByteString.Lazy          as BL


type Response = Web.ActionT Text Program ()


error :: Int -> Text -> Response
error statusCode =
  Web.status . Status.Status statusCode . BL.toStrict . TE.encodeUtf16BE


ping :: Response
ping = Web.text "PONG"


text :: Text -> Response
text = Web.text


json :: Json -> Response
json json_ = Web.setHeader "Content-Type" "application/json; charset=utf-8"
  >> encodeJson json_


encodeJson :: Json -> Response
encodeJson = Web.raw . TE.encodeUtf8 . Json.toText
