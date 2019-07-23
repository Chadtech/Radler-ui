module Data.BackendStatus exposing
    ( BackendStatus(..)
    , toColor
    )

import Colors
import Css exposing (Color)



-- TYPES --


type BackendStatus
    = Idle
    | Working



-- VIEW --


toColor : BackendStatus -> Color
toColor status =
    case status of
        Idle ->
            Colors.background1

        Working ->
            Colors.important0
