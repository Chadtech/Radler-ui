{-# LANGUAGE OverloadedStrings #-}


module Msg 
    ( Msg(..)
    ) 
    where

import Data.Route (Route(..))


data Msg
    = Request (Maybe Route)
