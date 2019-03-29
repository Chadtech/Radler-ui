module Data.Page exposing (Page(..))

import Data.Page.Parts as Parts



-- TYPES --


type Page
    = Package
    | Trackers
    | Parts (Maybe Parts.Model)
