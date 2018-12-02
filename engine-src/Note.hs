{-# LANGUAGE OverloadedStrings #-}


module Note
    ( Model
    , time
    , Note.read
    , Error
    , throw
    )
    where

import Data.Function
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Prelude.Extra (List, textToInt)
import Result (Result(Ok, Err))
import qualified Result 


data Model 
    = Model
        { time :: Int
        , randomSeed :: Int
        }


-- READ NOTE --


read :: Text -> Result Error (Model, Text)
read txt =
    case T.splitOn "," txt of
        timeTxt : randomSeedTxt : content : [] ->
            case (textToInt timeTxt, textToInt randomSeedTxt) of
                (Just time, Just randomSeed) ->
                    ( Model
                        { time = time
                        , randomSeed = randomSeed
                        }
                    , content
                    )
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
            T.concat
                [ "I was trying to parse a note, and I had trouble with this random seed and this time str respectively : " 
                , randomTxt 
                , " " 
                , timeTxt
                ]

        NoteStringHadWrongStructure txt ->
            T.append
                "Notes have three parts separated by \",\". This one was structured differently : "
                txt