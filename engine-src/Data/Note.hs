{-# LANGUAGE OverloadedStrings #-}


module Data.Note
    ( Note
    , readScore
    , Error
    , throw
    )
    where

import Data.Function
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
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
                & readRow
                & Result.andThen (addRow rest output)

        [] ->
            output
                & List.reverse
                & Ok


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
                & readNote
                & Result.andThen (addNote rest output)

        [] ->
            output
                & List.reverse
                & Ok


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
                        & Ok

                (Just _, Nothing) ->
                    randomSeedTxt
                        & CouldntParseRandomSeed
                        & Err

                (Nothing, Just _) ->
                    timeTxt
                        & CouldntParseTime
                        & Err

                (Nothing, Nothing) ->
                    CouldntParseAnything timeTxt randomSeedTxt
                        & Err

        _ ->
            txt
                & NoteStringHadWrongStructure
                & Err



-- ERROR --


data Error 
    = CouldntParseRandomSeed Text
    | CouldntParseTime Text
    | CouldntParseAnything Text Text
    | NoteStringHadWrongStructure Text


throw :: Error -> Text
throw error =
    case error of
        CouldntParseRandomSeed txt ->
            T.append
                "I was trying to parse a note, and I had trouble with this random seed : "
                txt

        CouldntParseTime txt ->
            T.append
                "I was trying to parse a note, and I had trouble with this time : "
                txt

        CouldntParseAnything timeTxt randomTxt ->
            [ "I was trying to parse a note, and I had trouble with this random seed and this time str respectively : " 
            , randomTxt 
            , " " 
            , timeTxt
            ]
                |> T.concat

        NoteStringHadWrongStructure txt ->
            T.append
                "Notes have three parts separated by \",\". This one was structured differently : "
                txt