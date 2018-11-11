module Cmd
    ( Cmd
    , none
    , toIO
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