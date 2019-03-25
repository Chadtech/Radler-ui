module Data.Modal.DeletePart exposing
    ( Model(..)
    , getPartIndex
    )

-- TYPES --


type Model
    = Ready { partIndex : Int }
    | Deleting
    | Error String



-- HELPERS --


getPartIndex : Model -> Maybe Int
getPartIndex model =
    case model of
        Ready { partIndex } ->
            Just partIndex

        _ ->
            Nothing
