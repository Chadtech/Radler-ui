module Data.Route exposing
    ( Route(..)
    , isPage
    , toPage
    , toString
    )

import Data.Page as Page exposing (Page)
import Page.Parts.Model as Parts



-- TYPES --


type Route
    = Package
    | Trackers
    | Parts



-- HELPERS --


toString : Route -> String
toString route =
    case route of
        Package ->
            "package"

        Trackers ->
            "trackers"

        Parts ->
            "parts"


toPage : Route -> Page
toPage route =
    case route of
        Package ->
            Page.Package

        Trackers ->
            Page.Trackers

        Parts ->
            Page.Parts Nothing


isPage : Page -> Route -> Bool
isPage page route =
    case ( page, route ) of
        ( Page.Package, Package ) ->
            True

        ( Page.Trackers, Trackers ) ->
            True

        ( Page.Parts _, Parts ) ->
            True

        _ ->
            False
