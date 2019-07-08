module Data.Tracker.Options exposing
    ( Model
    , init
    , setEveryField
    )

-- TYPES --


type alias Model =
    { collapseEveryField : Int }



-- INIT --


init : Int -> Model
init everyAmount =
    { collapseEveryField = everyAmount }


setEveryField : Int -> Model -> Model
setEveryField newEveryField model =
    { model | collapseEveryField = newEveryField }
