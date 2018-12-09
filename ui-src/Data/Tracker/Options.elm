module Data.Tracker.Options exposing
    ( Model
    , setCopyName
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


setCopyName : String -> Model -> Model
setCopyName copyName model =
    { model | copyName = copyName }


setMajorMarkField : String -> Model -> Model
setMajorMarkField str optionsModel =
    { optionsModel | majorMarkField = str }


setMinorMarkField : String -> Model -> Model
setMinorMarkField str optionsModel =
    { optionsModel | minorMarkField = str }
