module Data.Tracker
    exposing
        ( Tracker(..)
        , cellHeight
        , cellWidth
        , font
        )

import Css exposing (Style)
import Data.Sheet exposing (Sheet)
import Style


type Tracker
    = Big
    | Small


cellWidth : Tracker -> Float
cellWidth tracker =
    case tracker of
        Big ->
            90

        Small ->
            60


cellHeight : Tracker -> Float
cellHeight tracker =
    case tracker of
        Big ->
            26

        Small ->
            16


font : Tracker -> Style
font tracker =
    case tracker of
        Big ->
            Style.hfnss

        Small ->
            Style.hftin
