module Data.Project 
    ( Model
    , Error
    , throw
    , name
    , parts
    , fromString
    ) where


import qualified Data.Voice as Voice
import qualified Result
import Result (Result(Ok, Err))
import qualified Data.List as List
import Data.List.Split (splitOn)
import Text.Regex.Posix
import Flow
import qualified Util
import Prelude.Extra ((<<))


-- TYPES --


data Model =
    Model
    { name :: String
    -- , parts :: [ Part.Model ]
    , voices :: [ Voice.Model ] 
    , seed :: Int
    , timingVariance :: Int
    , beatLength :: Int
    }


type Parser a b =
    Result Error (a -> b) -> Result Error b


-- PUBLIC --


fromString :: String -> Result Error Model
fromString str =
    splitOn "\n" str
        |> toFields
        |> List.filter ((/=) "")
        |> List.map toKeyValues
        |> Result.join
        |> Result.andThen parse


-- PRIVATE --


parse :: [ (String, String) ] -> Result Error Model
parse fields =
    let
        ctor :: String -> (String -> Result Error a) -> Parser a b
        ctor key reader =
            Result.andThen 
                reader 
                (getField fields key)
                |> Result.map2 (|>)
    in
    Ok Model
        |> ctor "name" readName
        -- |> ctor "parts" (Result.mapError PartError << Part.readMany)
        |> ctor "voices" (Result.mapError VoiceError << Voice.readMany)
        |> ctor "seed" (useInt CouldNotParseSeed)
        |> ctor "timing-variance" (useInt CouldNotParseTimingVariance)
        |> ctor "beat-length" (useInt CouldNotParseBeatLength)


getField :: [ (String, String) ] -> String -> Result Error String
getField fields key =
    case fields of
        (thisKey, thisVal) : rest ->
            if thisKey == key then
                Ok thisVal
            else
                getField rest key

        [] ->
            Err (FieldDoesNotExist key)


readName :: String -> Result Error String
readName str =
    Ok str


useInt :: (String -> Error) -> String -> Result Error Int
useInt problemCtor str =
    let
        trimmedStr =
            Util.trim str
    in
    case Util.readInt trimmedStr of
        Just int ->
            Ok int

        Nothing ->
            problemCtor trimmedStr
                |> Err


toKeyValues :: String -> Result Error (String, String)
toKeyValues str =
    case splitOn "=" str of
        key : value : [] ->
            ( Util.dropLast (Util.trim key)
            , Util.trim value
            )
                |> Ok

        _ ->
            FieldIsntKeyValue str
                |> Err


toFields :: [ String ] -> [ String ]
toFields lines =
    case lines of
        first : (' ' : second) : rest ->
            toFields ((first ++ (' ' : second)) : rest)

        first : second : rest ->
            first : toFields (second : rest)

        _ ->
            lines


-- ERROR --


data Error
    = FieldIsntKeyValue String
    | FieldDoesNotExist String
    | CouldNotParseSeed String
    | CouldNotParseTimingVariance String
    | CouldNotParseBeatLength String
    -- | PartError Part.Error
    | VoiceError Voice.Error


throw :: Error -> String
throw error =
    "Project Error -> \n    " ++ errorToString error


errorToString :: Error -> String
errorToString error =
    case error of
        FieldIsntKeyValue str ->
            "Field isnt key value -> " ++ show str 

        FieldDoesNotExist str ->
            "Field does not exist -> " ++ show str

        CouldNotParseSeed str ->
            "could not parse into seed -> " ++ show str

        CouldNotParseTimingVariance str ->
            "could not parse timing variance -> " ++ show str

        CouldNotParseBeatLength str ->
            "could not parse beat length -> " ++ show str
