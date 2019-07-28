{-# LANGUAGE OverloadedStrings #-}


module Parse
    ( Decoder
    , Fields
    , Parse.apply
    , decode
    , get
    , fromDelimitedText
    , fromParameters
    , int
    , float
    , parse
    , decodeInt
    , fieldsToList
    ) where

import Flow
import Prelude.Extra

import Data.Attoparsec.Text ((<*.), (.*>))
import qualified Data.Attoparsec.Text as P
import qualified Data.Either.Extra as Either
import qualified Data.List as List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
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


data Fields v
    = Fields (Map TL.Text v)


instance Show t => Show (Fields t) where
    show (Fields fields) =
        show fields


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

    
{-| 
    Creates fields out of a string with key values 
    delimited by an equal sign
-}
fromDelimitedText :: Decoder a -> TL.Text -> Either TL.Text (Fields a)
fromDelimitedText decoder text =
    text
        |> TL.toStrict 
        |> P.parseOnly (P.many' (delimitedTextHelper decoder))
        |> Either.mapRight
            (Fields <. Map.fromList <. List.map (Tuple.first TL.fromStrict))
        |> Either.mapLeft TL.pack


delimitedTextHelper :: Decoder a -> P.Parser (T.Text, a)
delimitedTextHelper (Decoder parser) =
    (,)
        <$> (P.takeWhile (/= '=') <* "=") 
        <*> parser


{-|
    this will parse things that look like this

        a(b) c(d)

    into a `Fields Text` [("a", "b"), ("c", "d")]
-}
fromParameters :: TL.Text -> Either TL.Text (Fields TL.Text)
fromParameters text =
    text
        |> TL.toStrict 
        |> P.parseOnly (P.many' fieldsHelper)
        |> Either.mapRight listToFields
        |> Either.mapLeft TL.pack


listToFields :: List (T.Text, T.Text) -> Fields TL.Text
listToFields =
    Fields <. Map.fromList <. List.map (Tuple.both TL.fromStrict)


fieldsToList :: Fields v -> List (TL.Text, v)
fieldsToList (Fields map_) =
    Map.toList map_


fieldsHelper :: P.Parser (T.Text, T.Text)
fieldsHelper =
    (,)
        <$> (P.takeWhile isntParen) 
        <*> (P.string "(" *> P.takeWhile ((/=) ')') <* P.string ")")


isntParen :: Char -> Bool
isntParen c =
    c /= '(' && c /= ')'


get :: TL.Text -> Fields v -> Maybe v
get key (Fields map) =
    Map.lookup key map


int :: Decoder Int
int =
    Decoder <| P.signed P.decimal


float :: Decoder Float
float =
    Decoder <| realToFrac <$> P.signed P.double
