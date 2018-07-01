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
        , div
        )
import Html.Styled.Attributes exposing (css)


-- CONTAINER --


container : List (Attribute msg) -> List (Html msg) -> Html msg
container attrs =
    div (containerStyle :: attrs)


containerStyle : Attribute msg
containerStyle =
    [ margin2 zero auto ]
        |> css



-- ROW --


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs =
    div (rowStyle :: attrs)


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
    div (columnStyle :: attrs)


columnStyle : Attribute msg
columnStyle =
    [ flexBasis (pct 100)
    , boxSizing borderBox
    , flex (int 1)
    ]
        |> css
