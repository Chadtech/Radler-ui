module Cmd
    ( Cmd
    , none
    , batch
    , toIO
    , fromIO
    , Cmd.log
    )
    where

import Flow

import qualified Control.Monad as CM
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Prelude.Extra (List)


data Cmd
    = Cmd (IO ())
    | Batch (List Cmd)
    | None


none :: Cmd
none =
    None


batch :: List Cmd -> Cmd
batch =
    Batch


fromIO :: IO () -> Cmd
fromIO =
    Cmd


toIO :: Cmd -> IO ()
toIO cmd =
    case cmd of
        Cmd io ->
            io

        Batch cmds ->
            cmds
                |> List.map toIO
                |> CM.sequence_
            
        None ->
            return ()


log :: Text -> Cmd
log = Cmd <. putStrLn <. T.unpack