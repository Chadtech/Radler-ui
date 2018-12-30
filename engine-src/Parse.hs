{-# LANGUAGE OverloadedStrings #-}


module Parse
    ( Decoder
    , Fields
    , decode
    , getField
    , fields
    , int
    , parse
    , decodeInt
    ) where


import qualified Data.Attoparsec.Text as P
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Prelude.Extra (List, mapFirst)
import Result (Result(Ok, Err))
import qualified Result
    

parse :: 
    Result subError v
    -> (subError -> error)
    -> Result error (v -> w)
    -> Result error w
parse result errorCtor resultCtor =
    case resultCtor of
        Ok ctor ->
            case result of
                Ok v ->
                    Ok $ ctor v

                Err err ->
                    Err $ errorCtor err

        Err err ->
            Err err


data Fields a
    = Fields (List (TL.Text, a))


data Decoder a 
    = Decoder (P.Parser a)

decode :: Decoder a -> TL.Text -> Result TL.Text a
decode (Decoder parser) text =
    text 
        & TL.toStrict
        & P.parseOnly parser
        & Result.fromEither
        & Result.mapError TL.pack


decodeInt :: TL.Text -> Result TL.Text Int
decodeInt =
    decode int
 
fields :: Decoder a -> TL.Text -> Result TL.Text (Fields a)
fields decoder text =
    P.parseOnly 
        (P.many' (fieldsHelper decoder))
        (TL.toStrict text)
        & Result.fromEither
        & Result.map (Fields . List.map (mapFirst TL.fromStrict))
        & Result.mapError TL.pack


fieldsHelper :: Decoder a -> P.Parser (T.Text, a)
fieldsHelper (Decoder parser) =
    (,)
        <$> (P.takeWhile (/= '=') <* "=") 
        <*> parser


getField :: TL.Text -> Fields a -> Maybe a
getField key (Fields fields) =
    case fields of
        (firstKey, firstValue) : rest ->
            if firstKey == key then
                Just firstValue

            else
                getField key (Fields rest)

        [] ->
            Nothing


int :: Decoder Int
int =
    Decoder $ P.signed P.decimal
