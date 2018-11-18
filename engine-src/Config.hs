{-# LANGUAGE OverloadedStrings #-}


module Config
    ( Config
    , Config.read
    , Error
    , throw
    )
    where


import Data.Function ((&))
import Scale (Scale)
import qualified Scale
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (Parser)
import qualified Parse
import Result (Result(Ok, Err))
import qualified Result


-- TYPES --


data Config
    = Config
        { scale :: Scale }


-- HELPERS --


read :: Text -> Result Error Config
read txt =
    case T.splitOn ";" $ T.strip txt of
        scale : [] ->
            Config
                & Ok
                & applyScale scale


        _ ->
            UnexpectedConfigStructure txt
                & Err


applyScale :: Text -> Parser Error Scale b
applyScale txt ctorResult =
    case Scale.read txt of
        Ok scale ->
            Parse.construct scale ctorResult

        Err err ->
            Err (ScaleError err)


-- ERROR --


data Error 
    = UnrecognizedScale Text
    | UnexpectedConfigStructure Text
    | ScaleError Scale.Error


throw :: Error -> Text
throw error =
    case error of
        UnrecognizedScale text ->
            [ "I did not recognize this as an existing scale ->"
            , text
            ]
                & T.concat

        UnexpectedConfigStructure text ->
            [ "The structure of the config was not what I expected -> "
            , text
            ]
                & T.concat

        ScaleError scaleError ->
            Scale.throw scaleError