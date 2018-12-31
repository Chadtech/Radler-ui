{-# LANGUAGE OverloadedStrings #-}


module Size
    ( Size
    , width
    , Size.length
    , height
    , Size.read
    , Error
    , throw
    ) where

import Data.Function ((&))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Parse
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result 


-- TYPES --


data Size
    = Size
        { width :: Int
        , length :: Int
        , height :: Int
        }
        deriving (Eq)


data Part
    = Width
    | Length
    | Height


-- HELPERS --


read :: Parse.Fields Int -> Result Error Size
read roomFields =
    Result.mapError MissingPart (readFields roomFields)


readFields :: Parse.Fields Int -> Result Part Size
readFields fields =
    case Parse.getField "width" fields of
        Just width ->
            case Parse.getField "length" fields of
                Just length ->
                    case Parse.getField "height" fields of
                        Just height ->
                            Ok $ Size width length height

                        Nothing ->
                            Err Height

                Nothing ->
                    Err Length

        Nothing ->
            Err Width


partToText :: Part -> Text
partToText part =
    case part of
        Width ->
            "width"

        Length ->
            "length"

        Height ->
            "height"


-- ERROR --


data Error
    = MissingPart Part


throw :: Error -> Text
throw error =
    case error of
        MissingPart part ->
            [ "This part is missing -> "
            , partToText part
            ]
                & T.concat