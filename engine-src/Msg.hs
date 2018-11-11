{-# LANGUAGE OverloadedStrings #-}


module Msg 
    ( Msg(..)
    ) 
    where

import Route (Route(..))


data Msg
    = Request (Maybe Route)
