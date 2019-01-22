module Score.Diff 
    ( Difference

    ) where



import Score (Score)
import qualified Score


data Difference
    = NotSameName
    | NotSameConfig
    | NotSameNumberOfParts


diff :: Score -> Score -> Maybe Difference
diff incomingScore existingScore =
    if incomingScore == existingScore then
        Nothing

    else if Score.name incomingScore /= Score.name existingScore then
        Just NotSameName

    else if Score.config incomingScore /= Score.config existingScore then
        Just NotSameConfig

    else if not $ sameNumberOfParts incomingScore existingScore then
        Just NotSameNumberOfParts

    else
        -- TO DO
        Nothing        


sameNumberOfParts :: Score -> Score -> Bool
sameNumberOfParts incomingScore existingScore =
    Score.numberOfParts incomingScore /= Score.numberOfParts existingScore

