module Html.Grid
    exposing
        ( column
        , container
        , row
        )

import Css exposing (..)
import Css.Media as Media
    exposing
        ( only
        , screen
        , withMedia
        )
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , node
        )
import Html.Styled.Attributes exposing (css)


-- CONTAINER --


container : List Style -> List (Html msg) -> Html msg
container styles =
    node "container" [ css [ containerStyle, Css.batch styles ] ]


containerStyle : Style
containerStyle =
    [ margin2 zero auto ]
        |> Css.batch



-- ROW --


row : List Style -> List (Html msg) -> Html msg
row styles =
    node "row" [ css [ rowStyle, Css.batch styles ] ]


rowStyle : Style
rowStyle =
    [ displayFlex
    , flexDirection Css.row
    , boxSizing borderBox
    ]
        |> Css.batch



-- COLUMN --


column : List Style -> List (Html msg) -> Html msg
column styles =
    node "column" [ css [ columnStyle, Css.batch styles ] ]


columnStyle : Style
columnStyle =
    [ flexBasis (pct 100)
    , boxSizing borderBox
    , flex (int 1)
    ]
        |> Css.batch
