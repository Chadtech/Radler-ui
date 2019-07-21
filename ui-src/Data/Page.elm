module Data.Page exposing (Page(..))

import Data.Page.Parts as Parts
import Data.Page.Terminal as Terminal



-- TYPES --


type Page
    = Package
    | Trackers
    | Parts (Maybe Parts.Model)
    | Terminal
