{-# LANGUAGE OverloadedStrings #-}


module Note
    ( Model
    , time
    , Note.read
    , Error
    , throw
    )
    where

        
import Config (Config)
import qualified Config
import Data.Function
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result 


data Model 
    = Model
        { time :: Int
        , randomSeed :: Int
        }
        deriving (Eq)


-- READ NOTE --


read :: Config -> Text -> Result Error (Model, Text)
read config txt =
    case T.splitOn "," txt of
        timeTxt : randomSeedTxt : content : [] ->
            case (Parse.decodeInt timeTxt, Parse.decodeInt randomSeedTxt) of
                (Ok time, Ok randomSeed) ->
                    ( Model
                        { time = time + (Config.timingVariance config)
                        , randomSeed = randomSeed
                        }
                    , content
                    )
                        & Ok

                (Ok _, Err _) ->
                    randomSeedTxt
                        & CouldntParseRandomSeed
                        & Err

                (Err _, Ok _) ->
                    timeTxt
                        & CouldntParseTime
                        & Err

                (Err _, Err _) ->
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