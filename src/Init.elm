module Init
    exposing
        ( model
        , sheet
        , sheets
        )

import Array exposing (Array)
import Model exposing (Model)


sheets : Array (Array (Array String))
sheets =
    Array.fromList [ sheet ]


sheet : Array (Array String)
sheet =
    Array.repeat 6 ""
        |> Array.repeat 64


model : Model
model =
    { projectName = ""
    , sheets = sheets
    , smallViewSheet = 0
    , bigViewSheet = 0
    , majorMark = 16
    , minorMark = 4
    }
