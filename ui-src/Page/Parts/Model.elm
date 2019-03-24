module Page.Parts.Model exposing
    ( Model
    , init
    )

-- TYPES --


type alias Model =
    { selectedPartIndex : Maybe Int }


init : Model
init =
    { selectedPartIndex = Nothing }
