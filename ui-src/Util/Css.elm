module Util.Css exposing
    ( noStyle
    , styleIf
    )

import Css exposing (Style)


styleIf : Bool -> Style -> Style
styleIf condition style =
    if condition then
        style

    else
        noStyle


noStyle : Style
noStyle =
    Css.batch []
