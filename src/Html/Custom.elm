module Html.Custom
    exposing
        ( p
        )

import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        )
import Html.Styled.Attributes exposing (css)
import Style


p : List (Attribute msg) -> List (Html msg) -> Html msg
p attrs =
    Html.p (css [ Style.basicP ] :: attrs)
