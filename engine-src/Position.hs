module Position
    ( Position
    , x
    , y
    , Position.read
    ) where


import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Regex.Posix ((=~~))
import Result (Result(Ok, Err))
import qualified Result 


data Position
    = Position
        { x :: Int
        , y :: Int
        }
        deriving (Eq)


read :: Text -> Result Error Position
read maybePositionText =
    case (readX maybePositionText, readY maybePositionText) of
        (Just x, Just y) ->
            Ok $ Position x y

        (Just _, Nothing) ->
            Err XButNoY

        (Nothing, Just _) ->
            Err YButNoX

        (Nothing, Nothing) ->
            Err NoXOrY


readX :: Text -> Maybe Int
readX maybePositionText =
    T.unpack maybePositionText =~~ "x=(-?\\d+)"


readY :: Text -> Maybe Int
readY maybePositionText =
    T.unpack maybePositionText =~~ "y=(-?\\d+)"


-- ERROR --


data Error
    = XButNoY
    | YButNoX
    | NoXOrY