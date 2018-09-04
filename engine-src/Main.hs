module Main where

import qualified Terminal.Output as Output
import qualified Terminal.Input as Input
import qualified Data.ByteString as Byte
import Data.Text (Text)
import qualified Data.Text as T
import qualified Model
import Model (Model)
import qualified Data.List as List
import qualified Data.String as String
import qualified Result
import Result (Result(Err, Ok))
import Error (Error)
import qualified Error


main :: IO ()
main = do
    scoreData <- Byte.readFile "./project/score"
    awaitIfLoaded (Model.fromScoreData scoreData)


awaitIfLoaded :: Result Error Model -> IO ()
awaitIfLoaded result =
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

