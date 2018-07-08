module Html.Custom
    exposing
        ( Arrow(..)
        , onArrowKey
        , onEnter
        , p
        )

import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        )
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (keyCode, on)
import Json.Decode as D exposing (Decoder)
import Style


p : List (Attribute msg) -> List (Html msg) -> Html msg
p attrs =
    Html.p (css [ Style.basicP ] :: attrs)


type Arrow
    = Up
    | Down
    | Right
    | Left


onArrowKey : (Arrow -> msg) -> Attribute msg
onArrowKey ctor =
    D.map2 Tuple.pair
        (D.andThen fromIntArrowDecoder keyCode)
        (D.field "metaKey" D.bool)
        |> D.andThen ifMeta
        |> D.map ctor
        |> on "keydown"


fromIntArrowDecoder : Int -> Decoder Arrow
fromIntArrowDecoder key =
    case key of
        37 ->
            D.succeed Left

        39 ->
            D.succeed Right

        38 ->
            D.succeed Up

        40 ->
            D.succeed Down

        _ ->
            D.fail "Key isnt an arrow"


ifMeta : ( Arrow, Bool ) -> Decoder Arrow
ifMeta ( arrow, metaIsDown ) =
    if metaIsDown then
        D.succeed arrow
    else
        D.fail "Meta is now pressed"


onEnter : msg -> Attribute msg
onEnter msg =
    on "keydown" (keyCode |> D.andThen (enterDecoder msg))


enterDecoder : msg -> Int -> Decoder msg
enterDecoder msg keyCode =
    case keyCode of
        13 ->
            D.succeed msg

        _ ->
            D.fail "Not enter key"
