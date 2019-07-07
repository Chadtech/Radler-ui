module Data.Tracker.Options exposing
    ( Model
    , setMajorMarkField
    , setMinorMarkField
    )

-- TYPES --


type alias Model =
    { majorMarkField : String
    , minorMarkField : String
    , copyName : String
    }



-- HELPERS --


setMajorMarkField : String -> Model -> Model
setMajorMarkField str optionsModel =
    { optionsModel | majorMarkField = str }


setMinorMarkField : String -> Model -> Model
setMinorMarkField str optionsModel =
    { optionsModel | minorMarkField = str }
