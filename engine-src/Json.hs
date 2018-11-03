{-# LANGUAGE OverloadedStrings #-}


module Json
    ( Json
    , toText
    , object
    , string
    , Json.null
    , Json.maybe
    )
    where

import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Prelude.Extra (List)


data Json
    = Json Text


toText :: Json -> Text
toText (Json text) =
    text


string :: Text -> Json
string text =
    Json (quote text)


quote :: Text -> Text
quote text =
    T.concat [ "\"", text , "\"" ]


null :: Json
null =
    Json "null"


maybe :: Maybe Json -> Json
maybe maybeJson =
    case maybeJson of
        Just json ->
            json

        Nothing ->
            Json.null

object :: List (Text, Json) -> Json
object fields =
    Json $ T.concat $
        [ "{"
        , T.intercalate "," 
            $ List.map fieldToText
            $ fields
        , "}"
        ]



fieldToText :: (Text, Json) -> Text
fieldToText (key, Json value) =
    T.concat
        [ quote key
        , ":"
        , value
        ]