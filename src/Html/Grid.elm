module Html.Grid
    exposing
        ( column
        , columnWrapper
        , container
        , containerWrapper
        , row
        , rowWrapper
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


container : List (Attribute msg) -> List (Html msg) -> Html msg
container attrs =
    node "container" (containerStyle :: attrs)


containerWrapper : Html msg -> Html msg
containerWrapper child =
    container [] [ child ]


containerStyle : Attribute msg
containerStyle =
    [ margin2 zero auto ]
        |> css



-- ROW --


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs =
    node "row" (rowStyle :: attrs)


rowWrapper : Html msg -> Html msg
rowWrapper child =
    row [] [ child ]


rowStyle : Attribute msg
rowStyle =
    [ displayFlex
    , flexDirection Css.row
    , boxSizing borderBox
    ]
        |> css



-- COLUMN --


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs =
    node "column" (columnStyle :: attrs)


columnWrapper : Html msg -> Html msg
columnWrapper child =
    column [] [ child ]


columnStyle : Attribute msg
columnStyle =
    [ flexBasis (pct 100)
    , boxSizing borderBox
    , flex (int 1)
    ]
        |> css
