module Html.Buttons exposing
    ( delete
    , plus
    )

import Colors
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onClick)
import Style exposing (Size)


delete : msg -> List Style -> Size -> Html msg
delete handler extraStyles size =
    Html.button
        [ Attrs.css
            [ Style.clickableButtonStyle size
            , Css.batch extraStyles
            ]
        , onClick handler
        ]
        [ Html.text "x" ]


plus : msg -> List Style -> Size -> Html msg
plus handler extraStyles size =
    Html.button
        [ Attrs.css
            [ Style.clickableButtonStyle size
            , Css.batch extraStyles
            ]
        , onClick handler
        ]
        [ Html.text "+v" ]
