module Util exposing (..)

import Html.Styled as Html exposing (Html)


-- MAYBE --


{-|

    If a list of maybes contains only Justs
    then this function returns a Just of a
    list of those values

    [ Just 1, Just 2]   => Just [ 1, 2 ]
    [ Just 1, Nothing ] => Nothing

-}
allValues : List (Maybe a) -> Maybe (List a)
allValues list =
    case list of
        (Just v) :: vs ->
            Maybe.map ((::) v) (allValues vs)

        Nothing :: _ ->
            Nothing

        [] ->
            Just []



-- LIST --


unshift : List a -> a -> List a
unshift xs x =
    x :: xs
