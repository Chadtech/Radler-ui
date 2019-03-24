module Page.Parts exposing
    ( Msg
    , update
    , view
    )

import Colors
import Css exposing (..)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Page.Parts.Model as Parts
import Style



-- TYPES --


type Msg
    = PartClicked Int



-- UPDATE --


update : Msg -> Model -> Model
update msg model =
    case msg of
        PartClicked _ ->
            model



-- VIEW --


view : Model -> Parts.Model -> Html Msg
view model partsModel =
    Grid.container
        [ margin zero
        , padding (px 10)
        , width (pct 100)
        ]
        [ Grid.row
            [ height (pct 100) ]
            [ Grid.column
                [ flex3 (int 0) (int 1) (px 375) ]
                [ partsListView model ]
            , Grid.column
                []
                []
            ]
        ]


partsListView : Model -> Html Msg
partsListView model =
    Html.div
        [ Attrs.css
            [ Style.indent
            , backgroundColor Colors.background3
            , width (pct 100)
            ]
        ]
        [ model
            |> Model.indexedPartNames
            |> List.map partOptionView
            |> Grid.container []
        ]


partOptionView : ( Int, String ) -> Html Msg
partOptionView ( index, name ) =
    let
        style : List Style
        style =
            [ Style.hfnss
            , marginLeft (px 10)
            , cursor pointer
            , width (pct 100)
            , hover
                [ backgroundColor Colors.background4
                , color Colors.point1
                ]
            ]
    in
    Grid.row
        [ Style.basicSpacing ]
        [ Grid.column
            []
            [ Html.p
                [ Attrs.css style
                , Events.onClick (PartClicked index)
                ]
                [ Html.text name ]
            ]
        ]
