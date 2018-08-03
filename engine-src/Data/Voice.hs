module Data.Voice 
    ( Model
    , Error
    , readMany
    , throw
    ) 
    where


import Data.Int (Int16)
import Data.Note (Note)
import qualified Result
import Result (Result(Ok, Err))
import Flow
import Data.List as List
import Data.List.Split (splitOn)
import qualified Util


-- TYPES -- 


data Model 
    = P
    | N
    -- Model
    --     { name :: String }
    -- -- { compileNote :: Note -> [ Int16 ]
    -- -- -- , lineMaker :: [ (Int, Note) ] -> (Note -> [ Int16 ]) -> [ Int16 ]
    -- -- }


fromString :: String -> Result Error Model
fromString str =
    case Util.trim str of
        "p" ->
            Ok P

        "n" ->
            Ok N

        _ ->
            UnrecognizedVoiceType str
                |> Err


readMany :: String -> Result Error [ Model ]
readMany str =
    str
        |> splitOn ","
        |> List.map fromString
        |> Result.join


-- Error -- 


data Error
    = UnrecognizedVoiceType String


throw :: Error -> String
throw error =
    "Voice Error -> \n    " ++ errorToString error


errorToString :: Error -> String
errorToString error =
    case error of
        UnrecognizedVoiceType str ->
            "unrecognized voice type -> " ++ str