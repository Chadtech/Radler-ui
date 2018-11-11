{-# LANGUAGE OverloadedStrings #-}


module Score
    ( Score
    , fromText
    , Error
    , throw
    )
    where

import Data.Function
import Data.List as List
import Note (Note)
import qualified Note
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Voice as Voice
import Parse (Parser)
import qualified Parse
import Prelude.Extra (List)
import Result (Result(Ok, Err))
import qualified Result


data Score
    = Score
        { sourceText :: Text
        , name :: Text
        , voices :: List Voice.Model
        , notes :: List (List Note)
        }


fromText :: Text -> Result Error Score
fromText txt = 
    txt
        & toChunks
        & buildFromChunks txt
     


toChunks :: Text -> List Text
toChunks 
    = T.splitOn ":"
    . T.unlines
    . List.filter isntCommentLine
    . T.splitOn "\\n"
    . T.dropAround ((==) '"')


isntCommentLine :: Text -> Bool
isntCommentLine 
    = not
    . T.isPrefixOf "#"


buildFromChunks :: Text -> List Text -> Result Error Score
buildFromChunks scoreData chunks =
    case chunks of
        name : voices : notes : [] ->
            Score
                & Ok
                & Parse.construct scoreData
                & Parse.construct name
                & applyVoices voices
                & applyNotes notes

        _ ->
            Err (UnexpectedChunkStructure chunks)


applyNotes :: Text -> Parser Error (List (List Note)) b
applyNotes txt ctorResult =
    case Note.readScore txt of
        Ok score ->
            Parse.construct score ctorResult

        Err err ->
            Err (NoteError err)


applyVoices :: Text -> Parser Error (List Voice.Model) b
applyVoices txt ctorResult =
    case Voice.readMany txt of
        Ok voices ->
            Parse.construct voices ctorResult

        Err err ->
            Err (VoiceError err)


-- ERROR --


data Error
    = NoteError Note.Error
    | VoiceError Voice.Error
    | UnexpectedChunkStructure (List Text)


throw :: Error -> Text
throw error =
    case error of
        NoteError err ->
            Note.throw err

        VoiceError err ->
            Voice.throw err

        UnexpectedChunkStructure chunks ->
            [ "I could not parse the score. \
                \The chunks werent what I expected. : ->\n\n"
            , T.intercalate "chunk\n\n" chunks
            ]
                & T.concat