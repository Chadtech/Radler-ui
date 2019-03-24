module Page.Parts.Model exposing
    ( Model
    , init
    , setSelectedPartIndex
    )

-- TYPES --


type alias Model =
    { selectedPartIndex : Maybe Int }


init : Model
init =
    { selectedPartIndex = Nothing }



-- HELPERS --


setSelectedPartIndex : Int -> Model -> Model
setSelectedPartIndex newIndex model =
    { model | selectedPartIndex = Just newIndex }
