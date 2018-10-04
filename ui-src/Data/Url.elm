module Data.Url exposing
    ( Url
    , fromString
    , toString
    )


type Url
    = Url String


toString : Url -> String
toString (Url str) =
    str


fromString : String -> Url
fromString =
    Url
