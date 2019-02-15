{-# LANGUAGE OverloadedStrings #-}


module Contour
    (Contour
    ) where


import Flow
import Prelude.Extra


-- TYPES --


data Contour
    = FadeIn
    | FadeOut
    | FadeInAndOut
    | None


-- HELPERS --


read :: Text -> Either Error Contour
read text =
    case text of
        "i" ->
            Right FadeIn

        "o" ->
            Right FadeOut

        "n" ->
            Right None

        "" ->
            Right None

        "b" ->
            Right FadeInAndOut

        _ ->
            Left <| UnrecognizedContour text


-- ERROR --