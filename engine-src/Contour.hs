{-# LANGUAGE OverloadedStrings #-}


module Contour
  ( Contour
  , Contour.apply
  , Contour.read
  , Error
  , throw
  )
where


import           Flow
import           Prelude.Extra

import           Mono                           ( Mono )
import qualified Mono
import qualified Mono.Fade
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Timing


-- TYPES --


data Contour
    = FadeIn
    | FadeOut
    | FadeInAndOut
    | None
    deriving (Eq)


instance Show Contour where
  show contour = ["Contour ", toText contour] |> T.concat |> T.unpack


-- HELPERS --


toText :: Contour -> Text
toText contour = case contour of
  FadeIn       -> "fade in"

  FadeOut      -> "fade out"

  FadeInAndOut -> "fade in and out"

  None         -> "none"


apply :: Contour -> Mono -> Mono
apply contour mono = case contour of
  FadeIn  -> Mono.Fade.in_ Timing.Linear mono

  FadeOut -> Mono.Fade.out Timing.Linear mono

  FadeInAndOut ->
    mono |> Mono.Fade.in_ Timing.Linear |> Mono.Fade.out Timing.Linear

  None -> mono


read :: Text -> Either Error Contour
read text = case text of
  "i" -> Right FadeIn

  "o" -> Right FadeOut

  "n" -> Right None

  ""  -> Right None

  "b" -> Right FadeInAndOut

  _   -> Left <| UnrecognizedContour text


-- ERROR --


data Error
    = UnrecognizedContour Text
    deriving (Eq)


throw :: Error -> Text
throw error = case error of
  UnrecognizedContour text ->
    [ "This doesnt look like a Contour type I expected -> "
      , text
      , ", I expected \"i\", \"o\", \"n\", \"b\" or nothing"
      ]
      |> T.concat
