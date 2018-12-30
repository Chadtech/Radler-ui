module Result
    ( Result(..)
    , Result.map
    , apply
    , fromEither
    , mapError
    , map2
    , andThen
    , join
    )
    where


data Result e v
    = Ok v
    | Err e
    

fromEither :: Either e v -> Result e v
fromEither either =
    case either of
        Left err ->
            Err err

        Right value ->
            Ok value

        
apply :: a -> Result e (a -> b) -> Result e b
apply v result =
    case result of
        Ok f ->
            Ok $ f v

        Err err ->
            Err err


map :: (a -> b) -> Result e a -> Result e b
map f result =
    case result of
        Ok v ->
            Ok (f v)

        Err error ->
            Err error


mapError :: (a -> b) -> Result a v -> Result b v
mapError f result =
    case result of
        Ok v ->
            Ok v

        Err error ->
            Err (f error)


map2 :: (a -> b -> c) -> Result e a -> Result e b -> Result e c
map2 f ra rb =
    case (ra, rb) of
      (Ok a, Ok b) -> 
        Ok (f a b)

      (Err x, _) -> 
        Err x

      (_, Err x) ->
        Err x


andThen :: (a -> Result e b) -> Result e a -> Result e b
andThen f result =
    case result of
        Ok v ->
            f v

        Err problem ->
            Err problem


join :: [ Result e a ] -> Result e [ a ]
join results =
    joinHelp results []


joinHelp :: [ Result e a ] -> [ a ] -> Result e [ a ]
joinHelp results vs =
    case results of
        Ok v : rest ->
            joinHelp rest (v : vs)

        Err problem : _ ->
            Err problem

        [] ->
            Ok vs
