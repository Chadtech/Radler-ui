module Big
    exposing
        ( view
        )

import Array exposing (Array)
import Colors
import Css exposing (..)
import Data.Sheet exposing (Sheet)
import Html.Custom exposing (p)
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , div
        , input
        )
import Html.Styled.Attributes exposing (css)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style


-- VIEW --


view : Model -> Html Msg
view model =
    div
        [ css containerStyle ]
        (viewBody model)


containerStyle : List Style
containerStyle =
    [ Style.outdent
    , backgroundColor Colors.ignorable2
    , flex2 (int 1) (int 1)
    , margin (px 4)
    ]


viewBody : Model -> List (Html Msg)
viewBody model =
    case Array.get model.bigViewSheet model.sheets of
        Just sheet ->
            viewSheet sheet model

        Nothing ->
            [ p
                [ css [ Style.hfnss ] ]
                [ Html.text "Error : No Sheet" ]
            ]


viewSheet : Sheet -> Model -> List (Html Msg)
viewSheet sheet model =
    [ input
        [ css [ Style.basicInput ] ]
        []
    ]
