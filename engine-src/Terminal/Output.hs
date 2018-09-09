{-# LANGUAGE OverloadedStrings #-}


module Terminal.Output 
    ( send
    , UiMsg(..)
    ) where

import Cmd (Cmd)
import qualified Cmd
import Data.Text (Text)
import qualified Data.Text as T
import Flow
import System.Process (callCommand)


data UiMsg
    = Ready
    | EngineMsgNotRecognized Text


send :: UiMsg -> Cmd
send msg =
    case msg of
        Ready ->
            Cmd.withNoPayload "ready"

        EngineMsgNotRecognized engineMsg ->
            "engine msg not recognized"
                |> Cmd.withPayload engineMsg