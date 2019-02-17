module Data.Encoding exposing
    ( Backend(..)
    , None(..)
    )

{-| There is a basic structure to this application:

parts are a collection of beats
beats are a collection of notes
notes contain a string

That structure is used both in the ui, and how
we encode this information when sending it to
the back end

I dont want encoded notes to show up in the ui.
I just dont want it possible at all.

So. The Note type is a phantom type, and when it
gets encoded it goes from `Note Encoding.None` to
`Note Encoding.Backend`.

-}


type None
    = None


type Backend
    = Backend
