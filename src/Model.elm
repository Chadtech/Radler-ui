module Model exposing (Model)

import Array exposing (Array)


type alias Model =
    { projectName : String
    , sheets : Array (Array (Array String))
    , smallViewSheet : Int
    , bigViewSheet : Int
    , majorMark : Int
    , minorMark : Int
    }
