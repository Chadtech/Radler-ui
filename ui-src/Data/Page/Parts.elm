module Data.Page.Parts exposing
    ( Flags
    , Model
    , init
    , setCopyName
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


setCopyName : String -> Model -> Model
setCopyName str model =
    { model | copyName = str }
