{-# LANGUAGE OverloadedStrings #-}


module Update (update) where

-- import Cmd (Cmd)
-- import qualified Cmd
import Data.Response (Response)
import qualified Data.Response as Response
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Flow
import Msg (Msg(..))
import Model (Model)
import qualified Model
import System.Process (callCommand)
import Terminal.Output
    ( UiMsg
        ( EngineMsgNotRecognized
        , Ready
        )
    )
import qualified Terminal.Output as Output



update :: Msg -> Model -> (Model, Response)
update msg model =
    ( model
    , Response.ping
    )
    -- case msg of
    --     Play ->
    --         ( model
    --         , Cmd.none
    --         -- , Just (playIo model)
    --         )

    --     Build ->
    --         ( model
    --         , Cmd.none
    --         -- , Just buildIo
    --         )

    --     Init ->
    --         ( model
    --         , Output.send Ready
    --         )

    --     UnrecognizedCmd cmd ->
    --         ( model
    --         , cmd
    --             |> EngineMsgNotRecognized
    --             |> Output.send
    --         )


-- playIo :: Model -> IO ()
-- playIo model = do
--     Output.say "playing"
--     let audioFileName = getAudioFileName model
--     playCmd model
--     Output.newLine


-- playCmd :: Model -> IO ()
-- playCmd model =
--     model
--         |> Model.name
--         |> T.append "play "
--         |> T.unpack
--         |> callCommand


-- getAudioFileName :: Model -> Text
-- getAudioFileName model =
--     T.append (Model.name model) ".wav"


-- buildIo :: IO ()
-- buildIo = do
--     putStrLn "-- BUILDING"
--     Output.say "building"
--     Output.newLine

