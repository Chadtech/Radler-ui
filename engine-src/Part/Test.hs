{-# LANGUAGE OverloadedStrings #-}


module Part.Test
  ( Model
  , build
  , diff
  , fromTexts
  , toMono
  , Error
  , throw
  )
where


import           Flow
import           Prelude.Extra

import           Audio                          ( Audio )
import qualified Audio
import           Config                         ( Config )
import qualified Config
import qualified Data.Either.Extra             as Either
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Tuple.Extra              as Tuple
import           Mono                           ( Mono )
import qualified Mono
import qualified Note
import qualified Parse
import           Resolution                     ( Resolution )
import qualified Resolution
import qualified System.Random                 as Random
import           Time                           ( Time )
import qualified Time
import           Timeline                       ( Timeline )
import qualified Timeline


-- TYPES --


data Model
    = Model
        { notes :: Timeline Note }
        deriving (Eq)


instance Show Model where
  show model = model |> notes |> show


data Note
    = Note
        { amplitude :: Int }
        deriving (Eq)


instance Show Note where
  show note = show <| amplitude note


-- HELPERS --


diff :: Model -> Model -> Resolution Model
diff incomingModel existingModel =
  let isntNoteOf :: Model -> Time -> Note -> Bool
      isntNoteOf model time note = case Timeline.get time (notes model) of
        Just modelsNote -> modelsNote /= note

        Nothing         -> True
  in  ( mapNotes (Timeline.filterKey (isntNoteOf incomingModel)) existingModel
      , mapNotes (Timeline.filterKey (isntNoteOf existingModel)) incomingModel
      )
        |> Resolution.Changes


mapNotes :: (Timeline Note -> Timeline Note) -> Model -> Model
mapNotes f model = model { notes = f (notes model) }


fromTexts :: Config -> List Text -> Either Error Model
fromTexts config noteTexts =
  noteTexts |> fromTextsAccumulate config [] |> Either.mapRight
    (Model <. Timeline.fromList)


fromTextsAccumulate
  :: Config
  -> List (Time, Note)
  -> List Text
  -> Either Error (List (Time, Note))
fromTextsAccumulate config notes noteTexts = case noteTexts of
  first : rest -> case fromNoteText config first of
    Right (Just note) -> fromTextsAccumulate config (note : notes) rest

    Right Nothing     -> fromTextsAccumulate config notes rest

    Left  err         -> Left err

  [] -> Right notes


fromNoteText :: Config -> Text -> Either Error (Maybe (Time, Note))
fromNoteText config noteText = case Note.read config noteText of
  Right (time, seed, contentText) -> case contentText of
    "X" -> Right Nothing

    _   -> fromNonEmptyNoteText time seed contentText |> Either.mapRight Just

  Left error -> Left <| NoteError error


fromNonEmptyNoteText
  :: Time -> Random.StdGen -> Text -> Either Error (Time, Note)
fromNonEmptyNoteText time _ contentText =
  case Parse.decode Parse.int contentText of
    Right int -> Right (time, Note int)

    Left  _   -> Left <| NoteIsntInt contentText


toMono :: Model -> Mono
toMono model = model |> notes |> Timeline.map noteToMono |> Timeline.toMono


noteToMono :: Note -> Mono
noteToMono = Mono.singleton <. toFloat <. amplitude


build :: Model -> Audio
build = Audio.fromMono <. toMono


-- ERROR --


data Error
    = NoteError Note.Error
    | NoteIsntInt Text
    deriving (Eq)


throw :: Error -> Text
throw error = case error of
  NoteError   noteError -> Note.throw noteError

  NoteIsntInt text      -> T.append "This note is not an integer -> \n " text

