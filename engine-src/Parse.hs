module Parse
    ( Parser
    , construct
    ) where


import Result (Result)
import qualified Result
import Data.Function


type Parser error a b =
    Result error (a -> b) -> Result error b


construct :: a -> Result error (a -> b) -> Result error b
construct x =
    Result.map ((&) x)