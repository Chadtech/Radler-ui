module Parsing
    ( Parser
    ) where


import Result (Result)


type Parser error a b =
    Result error (a -> b) -> Result error b
