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


import Flow
import Prelude.Extra

import qualified Data.Either.Extra as Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Parse


-- TYPES --


data Size
    = Size
        { width :: Float
        , length :: Float
        , height :: Float
        }
        deriving (Eq)


data Part
    = Width
    | Length
    | Height


-- HELPERS --


read :: Parse.Fields Float -> Either Error Size
read roomFields =
    Either.mapLeft MissingPart <| readFields roomFields


readFields :: Parse.Fields Float -> Either Part Size
readFields fields =
    case Parse.getField "width" fields of
        Just width ->
            case Parse.getField "length" fields of
                Just length ->
                    case Parse.getField "height" fields of
                        Just height ->
                            Size width length height
                                |> Right

                        Nothing ->
                            Left Height

                Nothing ->
                    Left Length

        Nothing ->
            Left Width


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
                |> T.concat