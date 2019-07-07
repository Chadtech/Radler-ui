module Data.Page.Parts exposing
    ( Flags
    , Model
    , init
    , setCopyName
    )

import Data.Index exposing (Index)
import Data.Part exposing (Part)



-- TYPES --


type alias Model =
    { selectedPartIndex : Index Part
    , copyName : String
    }


type alias Flags =
    { selectedPartIndex : Index Part
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
