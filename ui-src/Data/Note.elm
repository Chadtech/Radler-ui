module Data.Note exposing
    ( Note
    , empty
    , encode
    , fromString
    , tests
    , toString
    )

import Data.Encoding as Encoding
import Expect
import Test exposing (Test, describe, test)


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


type Note encoding
    = Note String



-- HELPERS --


empty : Note Encoding.None
empty =
    fromString ""


toString : Note a -> String
toString (Note str) =
    str


fromString : String -> Note a
fromString =
    Note


encode : Int -> Int -> Note Encoding.None -> Note Encoding.Backend
encode time seed (Note str) =
    [ String.fromInt time
    , String.fromInt seed
    , encodeString str
    ]
        |> String.join dataDelimiter
        |> fromString


encodeTest : Test
encodeTest =
    test "Encode looks right" <|
        \_ ->
            "348080c"
                |> fromString
                |> encode 17 76
                |> Expect.equal (fromString "17,76,348080c")


dataDelimiter : String
dataDelimiter =
    ","


encodeString : String -> String
encodeString str =
    if String.isEmpty str then
        "X"

    else
        str



-- TESTS --


tests : Test
tests =
    describe "Data.Note"
        [ encodeTest ]
