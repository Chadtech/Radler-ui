module Model exposing (Model, Tracker(..), empty)

import Array exposing (Array)
import Data.Sheet as Sheet exposing (Sheet)


type alias Model =
    { projectName : String
    , sheets : Array Sheet
    , trackers : Array ( Int, Tracker )
    , majorMark : Int
    , minorMark : Int
    }


empty : Model
empty =
    { projectName = ""
    , sheets = Array.fromList [ Sheet.empty ]
    , trackers =
        [ ( 0, Small )
        , ( 0, Big )
        , ( 0, Big )
        , ( 0, Small )
        ]
            |> Array.fromList
    , majorMark = 16
    , minorMark = 4
    }


type Tracker
    = Big
    | Small
