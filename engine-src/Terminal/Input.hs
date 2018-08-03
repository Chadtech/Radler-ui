module Terminal.Input where


import qualified Data.ByteString as Byte 
    ( readFile
    , ByteString
    )
import qualified Data.ByteString.Char8 as Char
import Prelude (IO, String, getLine, putStrLn)
import Flow
import System.Process (callCommand)
import Data.List as List
import qualified Data.Project as Project
import Terminal.Output as Output
import Elmy
import qualified Model
import Model (Model)
import Update (update)
import Data.Maybe
import qualified Msg


enterCommand :: String -> IO ()
enterCommand projectName = do
    Output.separator
    putStrLn projectName
    putStrLn "Radler Engine"
    putStrLn "Enter Command"
    Output.separator
    Output.newLine


await :: Model -> IO ()
await model = do
    enterCommand (Model.name model)
    str <- getLine
    handleUpdate (update (Msg.fromString str) model)
    Output.newLine


handleUpdate :: (Model, Maybe (IO ())) -> IO ()
handleUpdate (model, maybeIo) =
    case maybeIo of
        Just io -> do
            io
            await model

        Nothing -> do
            await model

