{-# LANGUAGE OverloadedStrings #-}


module Terminal.Input
    ( init
    ) where

import Cmd (Cmd)
import qualified Cmd
import Data.Maybe
import qualified Data.Text as T
import Flow
import Model (Model)
import qualified Model
import qualified Msg
import Prelude (IO, String, getLine)
import Prelude hiding (init)
import Terminal.Output as Output
import Update (update)


await :: Model -> IO ()
await model = do
    str <- getLine
    model
        |> update (Msg.fromText (T.pack str))
        |> handleUpdate


init :: Model -> IO ()
init model =
    model
        |> update Msg.init
        |> handleUpdate


handleUpdate :: (Model, Cmd) -> IO ()
handleUpdate (model, cmd) =
    case Cmd.toIo cmd of
        Just io -> do
            io
            await model

        Nothing -> do
            await model

