{-# LANGUAGE OverloadedStrings #-}


module Update (update) where


import Msg (Msg(..))
import Model (Model)
import qualified Model
import System.Process (callCommand)
import qualified Terminal.Output as Output
import Data.Maybe
import Flow
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T


update :: Msg -> Model -> (Model, Maybe (IO ()))
update msg model =
    case msg of
        Play ->
            ( model
            , Just (playIo model)
            )

        Build ->
            ( model
            , Just buildIo
            )

        UnrecognizedCmd cmd ->
            ( model
            , Just notRecognizedIo
            )


playIo :: Model -> IO ()
playIo model = do
    Output.say "playing"
    let audioFileName = getAudioFileName model
    playCmd model
    Output.newLine


playCmd :: Model -> IO ()
playCmd model =
    model
        |> Model.name
        |> T.append "play "
        |> T.unpack
        |> callCommand


getAudioFileName :: Model -> Text
getAudioFileName model =
    T.append (Model.name model) ".wav"


buildIo :: IO ()
buildIo = do
    putStrLn "-- BUILDING"
    Output.say "building"
    Output.newLine


notRecognizedIo :: IO ()
notRecognizedIo = do
    putStrLn "-- Not recognized"
    Output.say "not recognized"
    Output.newLine

