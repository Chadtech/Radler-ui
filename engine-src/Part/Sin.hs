{-# LANGUAGE OverloadedStrings #-}


module Part.Sin
    ( Model
    , Part.Sin.read
    , Error
    , throw
    ) where


import Data.Function
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Note
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result 


data Model
    = Model
        { notes :: List Note }


data Note 
    = Note
        { note :: Note.Model
        , content :: Text
        }


read :: List Text -> Result Error Model
read 
    = Result.map Model
    . Result.join
    . List.map readNoteTxts


readNoteTxts :: Text -> Result Error Note
readNoteTxts noteTxt =
    case Note.read noteTxt of
        Ok (noteBase, contentTxt) ->
            Ok (Note noteBase contentTxt)

        Err error ->
            error
                & NoteError
                & Err


-- ERROR --


data Error 
    = NoteError Note.Error


throw :: Error -> Text
throw error =
    case error of
        NoteError noteError ->
            Note.throw noteError