module Data.Page exposing
    ( Page(..)
    , toString
    )

-- TYPES --


type Page
    = Package
    | Trackers
    | Parts



-- HELPERS --


toString : Page -> String
toString page =
    case page of
        Package ->
            "package"

        Trackers ->
            "trackers"

        Parts ->
            "parts"
