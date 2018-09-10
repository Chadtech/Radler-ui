{-# LANGUAGE OverloadedStrings #-}


module Cmd
    ( Cmd
    , none
    , toIo
    , withNoPayload
    , withPayload
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Flow
import Prelude hiding (init)


data Cmd = 
    Cmd (Maybe (IO ()))
        

none :: Cmd
none =
    Cmd Nothing


toIo :: Cmd -> Maybe (IO ())
toIo (Cmd maybeIo) =
    maybeIo


withPayload :: Text -> Text -> Cmd
withPayload payload type_ =
    [ type_
    , T.pack ";"
    , payload
    ]
        |> T.concat
        |> T.unpack
        |> putStrLn
        |> Just
        |> Cmd


withNoPayload :: Text -> Cmd
withNoPayload =
    withPayload ""
