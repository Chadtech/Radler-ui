module Style exposing
    ( basicSpacing
    , bigSpacing
    , dim
    , flush
    , font
    , fontSmoothingNone
    , globals
    , height
    , hfnss
    , hftin
    , indent
    , outdent
    , singleWidth
    , width
    )

import Colors
import Css exposing (..)
import Css.Global exposing (global)
import Data.Size as Size exposing (Size(..))
import Data.Width as Width exposing (Width)
import Html.Styled exposing (Html)



-- STYLES --


globals : Html msg
globals =
    [ Css.Global.p
        [ color Colors.point0
        , margin (px 0)
        , fontSmoothingNone
        , hfnss
        ]
    , Css.Global.input
        [ outline none
        , backgroundColor Colors.background1
        , fontSmoothingNone
        , indent
        , color Colors.point0
        , hfnss
        ]
    , Css.Global.textarea
        [ outline none
        , backgroundColor Colors.background1
        , indent
        ]
    , Css.Global.everything
        [ boxSizing borderBox
        , margin zero
        , padding zero
        ]
    ]
        |> global


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


dim : Style
dim =
    backgroundColor (rgba 0 0 0 0.5)


hfnss : Style
hfnss =
    [ fontFamilies [ "HFNSS" ]
    , fontSize (px 32)
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



-- HELPERS --


width : Size -> Width -> Style
width size width_ =
    case width_ of
        Width.Half ->
            ((Size.toUnitWidth size / 2) - 1)
                |> Css.px
                |> Css.width

        Width.Single ->
            Size.toUnitWidth size
                |> Css.px
                |> Css.width

        Width.Double ->
            ((Size.toUnitWidth size * 2) + 2)
                |> Css.px
                |> Css.width

        Width.Full ->
            Css.width <| Css.pct 100


singleWidth : Size -> Style
singleWidth size =
    width size Width.Single


height : Size -> Style
height =
    Css.height << Css.px << Size.toUnitHeight


font : Size -> Style
font size =
    case size of
        Big ->
            hfnss

        Small ->
            hftin
