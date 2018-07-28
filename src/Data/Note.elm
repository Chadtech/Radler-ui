module Data.Note
    exposing
        ( Note
        , empty
        , encodeTime
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


encodeTime : ( Int, Note ) -> Note
encodeTime ( time, Note str ) =
    [ String.fromInt time
    , str
    ]
        |> String.join ";"
        |> fromString
