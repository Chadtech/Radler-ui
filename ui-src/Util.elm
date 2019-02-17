module Util exposing
    ( allValues
    , mapCmd
    , mapModel
    , unshift
    , withModel
    , withNoCmd
    )

-- CMD --


mapCmd : (a -> b) -> ( model, Cmd a ) -> ( model, Cmd b )
mapCmd f ( model, cmd ) =
    ( model, Cmd.map f cmd )


withNoCmd : model -> ( model, Cmd msg )
withNoCmd model =
    ( model, Cmd.none )


withModel : model -> Cmd msg -> ( model, Cmd msg )
withModel =
    Tuple.pair


mapModel : (a -> b) -> ( a, Cmd msg ) -> ( b, Cmd msg )
mapModel =
    Tuple.mapFirst



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
