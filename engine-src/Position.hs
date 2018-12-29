module Position
    ( Position
    , x
    , y
    , Position.read
    , Error
    , throw
    ) where


import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Regex.Posix ((=~~))
import Result (Result(Ok, Err))
import qualified Result 


-- TYPES --


data Position
    = Position
        { x :: Int
        , y :: Int
        , z :: Int
        }
        deriving (Eq)


data Part 
    = X
    | Y
    | Z


-- HELPERS --


read :: Text -> Result Error Position
read positionText =
    case readPart X positionText of
        Just x ->
            case readPart Y positionText of
                Just y ->
                    case readPart Z positionText of
                        Just z ->
                            Ok $ Position x y z

                        Nothing ->
                            Err $ MissingPart Z

                Nothing ->
                    Err $ MissingPart Y

        Nothing ->
            Err $ MissingPart Z


    
readPart :: Part -> Text -> Maybe Int
readPart part positionText =
    T.unpack positionText =~~ (partToString part ++ "=(-?\\d+)")


partToString :: Part -> String
partToString part =
    case part of
        X ->
            "x"

        Y ->
            "y"

        Z ->
            "z"


-- ERROR --


data Error
    = MissingPart Part


throw :: Error -> Text
throw error =
    case error of
        MissingPart part ->
            T.append
                (T.pack "This part is missing -> ")
                (T.pack $ partToString part)