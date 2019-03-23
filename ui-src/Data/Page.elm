module Data.Page exposing
    ( Page(..)
    , toString
    )

import Data.Parts.Model as Parts



-- TYPES --


type Page
    = Package
    | Trackers
    | Parts Parts.Model



-- HELPERS --


toString : Page -> String
toString page =
    case page of
        Package ->
            "package"

        Trackers ->
            "trackers"

        Parts _ ->
            "parts"
