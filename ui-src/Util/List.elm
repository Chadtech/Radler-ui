module Util.List exposing (unshift)


unshift : List a -> a -> List a
unshift xs x =
    x :: xs
