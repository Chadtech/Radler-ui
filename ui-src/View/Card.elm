module View.Card exposing (config)

import Colors
import Css exposing (..)
import Html.Grid as Grid
import Html.Styled exposing (Html)
import Style


config : List Style -> List (Html msg) -> Html msg
config styles =
    Grid.box
        (cardStyles :: styles)


cardStyles : Style
cardStyles =
    [ Style.outdent
    , backgroundColor Colors.ignorable2
    , display inlineFlex
    , Style.basicSpacing
    ]
        |> Css.batch
