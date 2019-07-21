module Terminal
    ( Expression
    , fromText
    , Error
    , throw
    ) where


import Flow
import Prelude.Extra
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


-- TYPES --


data Expression
    = Unit


instance Show Expression where
    show expr =
        case expr of
            Unit ->
                "Unit"


-- HELPERS --


fromText :: Text -> Either Error Expression
fromText text =
    Right Unit


-- ERROR --


data Error
    = Error


throw :: Error -> Text
throw err =
    case err of
        Error ->
            "ERROR!!"