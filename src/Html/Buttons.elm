module Html.Buttons
    exposing
        ( delete
        , plus
        )

import Colors
import Css exposing (..)
import Html.Styled as Html
    exposing
        ( Html
        , button
        )
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Html.Styled.Events exposing (onClick)
import Style exposing (Size)


delete : msg -> Size -> Html msg
delete handler size =
    button
        [ css [ buttonStyleClickable size ]
        , onClick handler
        ]
        [ Html.text "x" ]


plus : msg -> List Style -> Size -> Html msg
plus handler extraStyles size =
    button
        [ css
            [ buttonStyleClickable size
            , Css.batch extraStyles
            ]
        , onClick handler
        ]
        [ Html.text "+v" ]


buttonStyleClickable : Size -> Style
buttonStyleClickable size =
    [ Style.basicButton size
    , active [ Style.indent ]
    , hover [ color Colors.point1 ]
    ]
        |> Css.batch
