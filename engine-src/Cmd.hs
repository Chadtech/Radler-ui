module Cmd
    ( Cmd
    , none
    , toIO
    , fromIO
    , Cmd.log
    )
    where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

data Cmd
    = Cmd (IO ())
    | None


none :: Cmd
none =
    None


fromIO :: IO () -> Cmd
fromIO =
    Cmd


toIO :: Cmd -> IO ()
toIO cmd =
    case cmd of
        Cmd io ->
            io

        None ->
            return ()


log :: Text -> Cmd
log 
    = Cmd
    . putStrLn
    . T.unpack