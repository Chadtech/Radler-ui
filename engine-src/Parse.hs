{-# LANGUAGE OverloadedStrings #-}


module Parse
    ( Decoder
    , Fields
    , Parse.apply
    , decode
    , getField
    , fields
    , int
    , float
    , parse
    , decodeInt
    ) where

import Flow
import Prelude.Extra

import qualified Data.Attoparsec.Text as P
import qualified Data.Either.Extra as Either
import qualified Data.List as List
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Tuple.Extra as Tuple
    

parse :: 
    Either subError v
    -> (subError -> error)
    -> Either error (v -> w)
    -> Either error w
parse result errorCtor resultCtor =
    case resultCtor of
        Right ctor ->
            case result of
                Right v ->
                    Right <| ctor v

                Left err ->
                    Left <| errorCtor err

        Left err ->
            Left err


apply :: a -> Either e (a -> b) -> Either e b
apply a either =
    case either of
        Right f ->
            Right <| f a

        Left err ->
            Left err


data Fields a
    = Fields (List (TL.Text, a))


data Decoder a 
    = Decoder (P.Parser a)


decode :: Decoder a -> TL.Text -> Either TL.Text a
decode (Decoder parser) text =
    text 
        |> TL.toStrict
        |> P.parseOnly parser
        |> Either.mapLeft TL.pack


decodeInt :: TL.Text -> Either TL.Text Int
decodeInt =
    decode int


fields :: Decoder a -> TL.Text -> Either TL.Text (Fields a)
fields decoder text =
    text
        |> TL.toStrict 
        |> P.parseOnly (P.many' (fieldsHelper decoder))
        |> Either.mapRight
            (Fields <. List.map (Tuple.first TL.fromStrict))
        |> Either.mapLeft TL.pack


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
    Decoder <| P.signed P.decimal


float :: Decoder Float
float =
    Decoder <| toFloat <$> P.signed P.decimal
