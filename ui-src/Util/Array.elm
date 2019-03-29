module Util.Array exposing
    ( insert
    , remove
    , tests
    )

import Array exposing (Array)
import Expect
import Test exposing (Test, describe, test)


remove : Int -> Array a -> Array a
remove index array =
    Array.append
        (Array.slice 0 index array)
        (Array.slice (index + 1) (Array.length array) array)


insert : Int -> a -> Array a -> Array a
insert index item array =
    Array.append
        (Array.push item (Array.slice 0 index array))
        (Array.slice index (Array.length array) array)



-- TESTS --


tests : Test
tests =
    describe "Array Util"
        [ test "remove" <|
            \_ ->
                remove 1 (Array.fromList [ 0, 1, 2 ])
                    |> Array.toList
                    |> Expect.equal [ 0, 2 ]
        , test "insert" <|
            \_ ->
                insert 1 9 (Array.fromList [ 0, 1, 2 ])
                    |> Array.toList
                    |> Expect.equal [ 0, 9, 1, 2 ]
        ]
