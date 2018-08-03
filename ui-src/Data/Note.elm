module Data.Note
    exposing
        ( Note
        , empty
        , encode
        , fromString
        , toString
        )

{-|

    Note :=
        As in a musical note. A note is
        a specification of some sound to
        play.

        In the context of this software,
        a Note is represented as a cell in
        a row in the grid UI.

-}

-- TYPES --


type Note
    = Note String



-- HELPERS --


empty : Note
empty =
    fromString ""


toString : Note -> String
toString (Note str) =
    str


fromString : String -> Note
fromString =
    Note


encode : Int -> Int -> Note -> Note
encode time seed ( Note str ) =
    [ String.fromInt time
    , String.fromInt seed
    , encodeString str
    ]
        |> String.join ";"
        |> fromString


encodeString : String -> String
encodeString str =
    if String.isEmpty str then
        "X"
    else
        str
