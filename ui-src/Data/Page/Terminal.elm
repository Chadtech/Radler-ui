module Data.Page.Terminal exposing
    ( Model
    , init
    )


type alias Model =
    { text : String }


init : Model
init =
    { text = "" }
