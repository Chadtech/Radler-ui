module Style
    exposing
        ( basicInput
        , basicP
        , fontSmoothingNone
        , hfnss
        , hftin
        , indent
        , outdent
        )

import Colors
import Css exposing (..)
import Html.Styled.Attributes exposing (css)


basicInput : Style
basicInput =
    [ outline none
    , backgroundColor Colors.background3
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
