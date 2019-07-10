{-# LANGUAGE OverloadedStrings #-}


module Room
  ( Room
  , listenerPosition
  , mapListenerPosition
  , maybeFromText
  , size
  , multiplyBy
  , Error
  , throw
  )
where


import           Flow

import qualified Data.Either.Extra             as Either
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import           Parse                          ( parse )
import qualified Parse
import           Position                       ( Position )
import qualified Position
import qualified Size
import           Size                           ( Size )


-- TYPES --


--        |------------|
--        |            |
--        |            |
--        |            |
--        |            |
--       ^|            |
--       ||            |
--  length|------------|
--        width ->



data Room
    = Room
        { listenerPosition :: Position
        , size :: Size
        }
        deriving (Eq)


instance Show Room where
  show room =
    [ "Room : "
      , T.pack <| show (listenerPosition room)
      , T.pack <| show (size room)
      ]
      |> T.concat
      |> T.unpack


-- HELPERS --


maybeFromText :: Text -> Either Error (Maybe Room)
maybeFromText roomText = if roomText == "no-room"
  then Right Nothing
  else fromText roomText |> Either.mapRight Just


fromText :: Text -> Either Error Room
fromText roomText = case Parse.fromDelimitedText Parse.float roomText of
  Right fields ->
    Room
      |> Right
      |> parse (Position.read fields)        PositionError
      |> parse (Size.fromFloatFields fields) SizeError

  Left err -> Left <| ParsingFailed err


mapListenerPosition :: (Position -> Position) -> Room -> Room
mapListenerPosition f room =
  room { listenerPosition = f (listenerPosition room) }


multiplyBy :: Int -> Room -> Room
multiplyBy factor room =
  let
    listenerPos :: Position
    listenerPos = listenerPosition room

    roomSize :: Size
    roomSize = size room

    newRoomSize :: Size
    newRoomSize = Size.multiplyBy factor roomSize
  in
    Room
      (Position.fromCoords
        ( Position.x listenerPos / Size.width roomSize * Size.width newRoomSize
        , Position.y listenerPos
        / Size.length roomSize
        * Size.length newRoomSize
        , Position.z listenerPos
        / Size.height roomSize
        * Size.height newRoomSize
        )
      )
      newRoomSize


-- ERROR --


data Error
    = PositionError Position.Error
    | SizeError Size.Error
    | ParsingFailed Text
    deriving (Eq)


throw :: Error -> Text
throw error = case error of
  PositionError error -> Position.throw error

  SizeError     error -> Size.throw error

  ParsingFailed error -> T.append "Parsing failed - > " error


