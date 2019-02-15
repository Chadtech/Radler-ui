{-# LANGUAGE OverloadedStrings #-}


module Contour
    ( Contour
    , Contour.apply
    , Contour.read
    , Error
    , throw
    ) where


import Flow
import Prelude.Extra

import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


-- TYPES --


data Contour
    = FadeIn
    | FadeOut
    | FadeInAndOut
    | None
    deriving (Eq)


-- HELPERS --

apply :: Contour -> Mono -> Mono
apply contour mono =
    case contour of
        FadeIn ->
            Mono.fadeIn mono

        FadeOut ->
            Mono.fadeOut mono

        FadeInAndOut ->
            mono
                |> Mono.fadeIn
                |> Mono.fadeOut

        None ->
            mono


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


data Error
    = UnrecognizedContour Text


throw :: Error -> Text
throw error =
    case error of
        UnrecognizedContour text ->
            [ "This doesnt look like a Contour type I expected -> "
            , text
            , ", I expected \"i\", \"o\", \"n\", \"b\" or nothing"
            ]
                |> T.concat
