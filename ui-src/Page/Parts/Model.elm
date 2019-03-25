module Page.Parts.Model exposing
    ( Flags
    , Model
    , init
    , setCopyName
    , setSelectedPartIndex
    )

-- TYPES --


type alias Model =
    { selectedPartIndex : Int
    , copyName : String
    }


type alias Flags =
    { selectedPartIndex : Int
    , copyName : String
    }


init : Flags -> Model
init flags =
    { selectedPartIndex = flags.selectedPartIndex
    , copyName = flags.copyName
    }



-- HELPERS --


setSelectedPartIndex : Int -> Model -> Model
setSelectedPartIndex newIndex model =
    { model | selectedPartIndex = newIndex }


setCopyName : String -> Model -> Model
setCopyName str model =
    { model | copyName = str }
