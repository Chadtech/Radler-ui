module Style exposing (..)

import Colors
import Css exposing (..)
import Html.Styled.Attributes exposing (css)


-- TYPES --


type Size
    = Big
    | Small



-- STYLES --


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


flush : Style
flush =
    border3 (px 2) solid Colors.ignorable3


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


bigSpacing : Style
bigSpacing =
    [ padding (px 5)
    , margin (px 5)
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


basicButton : Size -> Style
basicButton size =
    [ outdent
    , font size
    , margin (px 1)
    , width (px (noteWidth size / 2))
    , height (px (noteHeight size))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , fontSmoothingNone
    , padding (px 0)
    , outline none
    ]
        |> Css.batch



-- HELPERS --


noteWidth : Size -> Float
noteWidth size =
    case size of
        Big ->
            90

        Small ->
            60


noteHeight : Size -> Float
noteHeight size =
    case size of
        Big ->
            26

        Small ->
            16


font : Size -> Style
font size =
    case size of
        Big ->
            hfnss

        Small ->
            hftin