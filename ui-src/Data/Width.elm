module Data.Width exposing
    ( Width(..)
    , double
    , full
    , half
    , single
    )

-- TYPES --


type Width
    = Half
    | Single
    | Double
    | Full



-- VALUES --


half : Width
half =
    Half


single : Width
single =
    Single


double : Width
double =
    Double


full : Width
full =
    Full
