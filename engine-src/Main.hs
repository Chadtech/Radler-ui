{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Main (main) where

import Prelude.Extra
import Flow

import qualified Audio
import Audio (Audio)


-- MAIN --


main :: IO ()
main =
    Audio.read "./elm-europe-audio/travel.wav"
        |> andThen fromAudio


fromAudio :: Either Audio.ReadError Audio -> IO ()
fromAudio result =
    case result of
        Left _ ->
            putStrLn "Error"

        Right audio ->
            Audio.write "elm-europe-snare.wav" audio




--
-- import Data.Text.Lazy (Text)
-- import qualified Model
-- import Network.Wai.Middleware.RequestLogger as NWMR
-- import Program (Program)
-- import qualified Program
-- import qualified Router
-- import Web.Scotty.Trans (ScottyT)
-- import qualified Web.Scotty.Trans as Web
--
--
-- -- MAIN --
--
--
-- main :: IO ()
-- main =
--     Program.init Model.init router
--
--
-- -- ROUTER --
--
--
-- router :: ScottyT Text Program ()
-- router =
--     -- Web.middleware NWMR.logStdoutDev
--         Router.get "/ping"
--         >> Router.post "/echo"
--         >> Router.post "/play"
--         >> Router.post "/build"
--
--



