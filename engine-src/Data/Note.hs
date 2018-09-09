{-# LANGUAGE OverloadedStrings #-}


module Data.Note
    ( Note
    , readScore
    , Error
    , throw
    )
    where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Flow
import Prelude.Extra (List, textToInt, debugLog)
import Result (Result(Ok, Err))
import qualified Result 


data Note 
    = Note
        { time :: Int
        , randomSeed :: Int
        , content :: Text
        }


-- READ SCORE --


readScore :: Text -> Result Error (List (List Note))
readScore txt =
    readScoreAccumulate 
        (T.splitOn "\n" (T.strip txt)) 
        []


readScoreAccumulate :: List Text -> List (List Note) -> Result Error (List (List Note))
readScoreAccumulate rowTexts output =
    case rowTexts of
        first : rest ->
            first
                |> readRow
                |> Result.andThen (addRow rest output)

        [] ->
            output
                |> List.reverse
                |> Ok


addRow :: List Text -> List (List Note) -> List Note -> Result Error (List (List Note))
addRow rowTexts outputRows thisRow =
    readScoreAccumulate rowTexts (thisRow : outputRows)


-- READ ROW --


readRow :: Text -> Result Error (List Note)
readRow rowText =
    readRowAccumulate (T.splitOn ";" rowText) []


readRowAccumulate :: List Text -> List Note -> Result Error (List Note)
readRowAccumulate noteTexts output =
    case noteTexts of
        first : rest ->
            first
                |> readNote
                |> Result.andThen (addNote rest output)

        [] ->
            output
                |> List.reverse
                |> Ok


addNote :: List Text -> List Note -> Note -> Result Error (List Note)
addNote noteTexts outputNotes thisNote =
    readRowAccumulate noteTexts (thisNote : outputNotes)


-- READ NOTE --


readNote :: Text -> Result Error Note
readNote txt =
    case T.splitOn "," txt of
        timeTxt : randomSeedTxt : content : [] ->
            case (textToInt timeTxt, textToInt randomSeedTxt) of
                (Just time, Just randomSeed) ->
                    Note
                        { time = time
                        , randomSeed = randomSeed
                        , content = content
                        }
                        |> Ok

                (Just _, Nothing) ->
                    randomSeedTxt
                        |> T.unpack
                        |> CouldntParseRandomSeed
                        |> Err

                (Nothing, Just _) ->
                    timeTxt
                        |> T.unpack
                        |> CouldntParseTime
                        |> Err

                (Nothing, Nothing) ->
                    CouldntParseAnything
                        (T.unpack timeTxt)
                        (T.unpack randomSeedTxt)
                        |> Err

        _ ->
            txt
                |> T.unpack
                |> NoteStringHadWrongStructure
                |> Err



-- ERROR --


data Error 
    = CouldntParseRandomSeed String
    | CouldntParseTime String
    | CouldntParseAnything String String
    | NoteStringHadWrongStructure String

throw :: Error -> String
throw error =
    case error of
        CouldntParseRandomSeed str ->
            "I was trying to parse a note, and I had trouble with this random seed : " ++ str

        CouldntParseTime str ->
            "I was trying to parse a note, and I had trouble with this time : " ++ str

        CouldntParseAnything timeStr randomStr ->
            "I was trying to parse a note, and I had trouble with this random seed and this time str respectively : " ++ randomStr ++ " " ++ timeStr

        NoteStringHadWrongStructure str ->
            "Notes have three parts separated by \",\". This one was structured differently : " ++ str