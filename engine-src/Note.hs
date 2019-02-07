{-# LANGUAGE OverloadedStrings #-}


module Note
    ( Model
    , Note.read
    , Error
    , throw
    )
    where


import Flow
import Prelude.Extra

import Config (Config)
import qualified Config
import Data.Function
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse
import Time (Time)
import qualified Time


data Model 
    = Model
        { randomSeed :: Int }
        deriving (Eq)


-- READ NOTE --


read :: Config -> Text -> Either Error ((Time, Model), Text)
read config txt =
    case T.splitOn "," txt of
        timeTxt : randomSeedTxt : content : [] ->
            case (Parse.decodeInt timeTxt, Parse.decodeInt randomSeedTxt) of
                (Right time, Right randomSeed) ->
                    ( ( time + (Config.timingVariance config)
                        |> Time.fromInt
                      , Model { randomSeed = randomSeed }
                      )
                    , content
                    )
                        |> Right

                (Right _, Left _) ->
                    randomSeedTxt
                        |> CouldntParseRandomSeed
                        |> Left

                (Left _, Right _) ->
                    timeTxt
                        |> CouldntParseTime
                        |> Left

                (Left _, Left _) ->
                    CouldntParseAnything timeTxt randomSeedTxt
                        |> Left

        _ ->
            txt
                |> NoteStringHadWrongStructure
                |> Left



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