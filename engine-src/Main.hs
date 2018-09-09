module Main where

import qualified Data.ByteString as Byte
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.String as String
import qualified Data.Text as T
import Error (Error)
import qualified Error
import Model (Model)
import qualified Model
import Prelude hiding (init)
import qualified Terminal.Input as Input
import qualified Terminal.Output as Output
import Result (Result(Err, Ok))
import qualified Result


main :: IO ()
main = do
    scoreData <- Byte.readFile "./project/score"
    init (Model.init scoreData)


init :: Result Error Model -> IO ()
init result =
    case result of
        Ok model -> do
            ready (Model.name model)
            Input.await model

        Err err -> do
            putStrLn (Error.throw err)


ready :: Text -> IO ()
ready txt = do
    _ <- Output.say ("ready " ++ (T.unpack txt))
    Output.newLine
    putStrLn "Ready"
    Output.newLine

