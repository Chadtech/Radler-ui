module Elmy where


head :: [ a ] -> Maybe a
head list =
    case list of
        [] ->
            Nothing

        first : rest ->
            Just first


type List a = [ a ]


infixl 0 <<
(<<) = leftCompose


leftCompose :: (b -> c) -> (a -> b) -> a -> c
leftCompose f g v =
    f (g v)