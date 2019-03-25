module Data.Page exposing (Page(..))

import Page.Parts.Model as Parts



-- TYPES --


type Page
    = Package
    | Trackers
    | Parts (Maybe Parts.Model)
