module Data.Error exposing
    ( Error(..)
    , toString
    )

-- TYPES --


type Error
    = ScoreDidNotSave
    | BackendHadProblemWithScore String



-- HELPERS --


toString : Error -> String
toString error =
    case error of
        ScoreDidNotSave ->
            """
            The score failed to save.
            This is probably because the score includes
            a part that is not loaded into Radler
            """

        BackendHadProblemWithScore str ->
            str
