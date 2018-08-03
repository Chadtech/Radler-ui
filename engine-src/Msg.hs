module Msg (Msg(..), fromString) where


import Result (Result(..))
import Data.List.Split (splitOn)


data Msg
    = Play
    | Build
    | ShowParts
    | UnrecognizedCmd String


fromString :: String -> Msg
fromString str =
    case splitOn " " str of
        "build" : _ ->
            Build

        "play" : _ ->
            Play

        "show" : "parts" : _ ->
            ShowParts

        _ ->
            UnrecognizedCmd str
