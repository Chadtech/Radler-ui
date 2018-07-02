module Style exposing (..)

import Colors
import Css exposing (..)
import Html.Styled.Attributes exposing (css)


basicInput : Style
basicInput =
    [ outline none
    , backgroundColor Colors.background1
    , indent
    ]
        |> Css.batch


indent : Style
indent =
    [ borderTop3 (px 2) solid Colors.ignorable3
    , borderLeft3 (px 2) solid Colors.ignorable3
    , borderRight3 (px 2) solid Colors.ignorable1
    , borderBottom3 (px 2) solid Colors.ignorable1
    ]
        |> Css.batch


outdent : Style
outdent =
    [ borderTop3 (px 2) solid Colors.ignorable1
    , borderLeft3 (px 2) solid Colors.ignorable1
    , borderRight3 (px 2) solid Colors.ignorable3
    , borderBottom3 (px 2) solid Colors.ignorable3
    ]
        |> Css.batch


hfnss : Style
hfnss =
    [ fontFamilies [ "HFNSS" ]
    , fontSize (px 32)
    ]
        |> Css.batch


basicP : Style
basicP =
    [ color Colors.point0
    , margin (px 0)
    , fontSmoothingNone
    ]
        |> Css.batch


fontSmoothingNone : Style
fontSmoothingNone =
    property "-webkit-font-smoothing" "none"


hftin : Style
hftin =
    [ fontFamilies [ "HFTIN" ]
    , fontSize (px 16)
    ]
        |> Css.batch


basicSpacing : Style
basicSpacing =
    [ padding (px 1)
    , margin (px 1)
    ]
        |> Css.batch


card : Style
card =
    [ outdent
    , backgroundColor Colors.ignorable2
    , display inlineFlex
    , basicSpacing
    ]
        |> Css.batch
