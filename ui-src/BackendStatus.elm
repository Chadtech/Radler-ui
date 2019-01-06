module BackendStatus exposing
    ( BackendStatus(..)
    , view
    )

import Colors
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs
import Style


type BackendStatus
    = Idle
    | Working


view : BackendStatus -> Html msg
view status =
    Html.input
        [ Attrs.css
            [ backgroundColor <| statusToColor status
            , margin (px 1)
            , height (pct 100)
            ]
        ]
        []


statusToColor : BackendStatus -> Color
statusToColor status =
    case status of
        Idle ->
            Colors.background1

        Working ->
            Colors.important0
