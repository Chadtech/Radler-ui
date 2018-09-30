{-# LANGUAGE OverloadedStrings #-}


module Msg 
    ( Msg(..)
    -- , init
    -- , fromText
    ) 
    where

import Data.Text (Text)
import qualified Data.Text as T        
import Prelude hiding (init)
import Result (Result(..))
import Data.Route (Route(..))


data Msg
    = ChangeTestTxt
    | Request (Maybe Route)




-- data Msg
--     = Play
--     | Build
--     | Init
--     | UnrecognizedCmd Text


-- init :: Msg
-- init =
--     Init


-- fromText :: Text -> Msg
-- fromText txt =
--     case T.splitOn " " txt of
--         "build" : _ ->
--             Build

--         "play" : _ ->
--             Play

--         _ ->
--             UnrecognizedCmd txt
