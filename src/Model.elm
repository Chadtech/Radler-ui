module Model exposing (Model, empty)

import Array exposing (Array)
import Data.Sheet as Sheet exposing (Sheet)


type alias Model =
    { projectName : String
    , sheets : Array Sheet
    , smallViewSheet : Int
    , bigViewSheet : Int
    , majorMark : Int
    , minorMark : Int
    }


empty : Model
empty =
    { projectName = ""
    , sheets =
        Array.fromList
            [ Sheet.empty ]
    , smallViewSheet = 0
    , bigViewSheet = 0
    , majorMark = 16
    , minorMark = 4
    }
