module Main where

import qualified Data.ByteString as Byte
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Error (Error)
import qualified Error
import Flow
import Model (Model)
import qualified Model
import Prelude hiding (init)
import qualified Terminal.Input as Input
import Result (Result(Err, Ok))
import qualified Result

main :: IO ()
main = do
    scoreData <- Byte.readFile "./project/score"
    scoreData
        |> TE.decodeUtf8
        |> Model.init
        |> init


init :: Result Error Model -> IO ()
init result =
    case result of
        Ok model -> 
            Input.init model

        Err err -> do
            putStrLn (Error.throw err)
    
