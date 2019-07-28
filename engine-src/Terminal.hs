module Terminal
    ( Action
    , fromText
    , Error
    , throw
    ) where


import Flow
import Prelude.Extra

import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Parse
import qualified Control.Monad as CM


-- TYPES --


data Action
    = Unit


instance Show Action where
    show expr =
        case expr of
            Unit ->
                "Unit"


-- HELPERS --


fromText :: Text -> Either Error (List Action)
fromText txt =
    case Parse.fromParameters txt of
        Right params ->
            params
                |> Parse.fieldsToList
                |> List.map fromField
                |> CM.sequence

        Left error ->
            Left <| ParseError error


fromField :: (Text, Text) -> Either Error Action
fromField (key, value) =
    Right Unit


-- ERROR --


data Error
    = ParseError Text


throw :: Error -> Text
throw err =
    case err of
        ParseError error ->
            T.append "Parse Error -> " error