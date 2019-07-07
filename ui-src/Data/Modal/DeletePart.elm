module Data.Modal.DeletePart exposing
    ( Model(..)
    , getPartIndex
    )

import Data.Index exposing (Index)
import Data.Part exposing (Part)



-- TYPES --


type Model
    = Ready (Index Part)
    | Deleting
    | Error String



-- HELPERS --


getPartIndex : Model -> Maybe (Index Part)
getPartIndex model =
    case model of
        Ready partIndex ->
            Just partIndex

        _ ->
            Nothing
