module Util exposing (..)

import Html.Styled as Html exposing (Html)


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf condition f =
    if condition then
        f ()
    else
        Html.text ""
