module Model 
    ( Model
    , name
    -- , project
    , fromScoreData
    )
    where


import qualified Data.Part as Part
import qualified Data.Project as Project
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
import Result (Result(Ok))
import qualified Result
import qualified Data.List as List
import qualified Data.ByteString.Char8 as Char
import Flow
import Error (Error(ProjectError))


data Model
    = Model
        { lastScoreStr :: String 
        , name :: String
        }
        -- { project :: Project.Model
        -- }


fromScoreData :: ByteString -> Result Error Model
fromScoreData byteString =
    byteString
        |> getScoreString
        |> fromScoreString
        |> Ok
        -- |> Project.fromString
        -- |> Result.mapError ProjectError
        -- |> Result.map fromProject


fromScoreString :: String -> Model
fromScoreString scoreStr =
    Model
        { lastScoreStr = scoreStr 
        , name = "WOW"
        }


getScoreString :: ByteString -> String
getScoreString scoreData = 
    scoreData
        |> Char.split '\n'
        |> List.map Char.unpack
        |> List.unlines