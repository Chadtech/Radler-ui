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
import Util



-- TYPES --


type Msg
    = PartClicked Int



-- UPDATE --


update : Msg -> Parts.Model -> Model -> Model
update msg partsModel model =
    case msg of
        PartClicked newIndex ->
            Model.setPartsPage
                (Parts.setSelectedPartIndex newIndex partsModel)
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
                [ partsListView model partsModel ]
            , Grid.column
                []
                [ partView model partsModel ]
            ]
        ]



-- PART VIEW --


partView : Model -> Parts.Model -> Html Msg
partView model partsModel =
    let
        noPartSelected : Html Msg
        noPartSelected =
            Html.p
                []
                [ Html.text "No part selected" ]
    in
    case partsModel.selectedPartIndex of
        Just index ->
            Html.text <| String.fromInt index

        Nothing ->
            noPartSelected



-- PARTS LIST VIEW --


partsListView : Model -> Parts.Model -> Html Msg
partsListView model partsModel =
    Html.div
        [ Attrs.css
            [ Style.indent
            , backgroundColor Colors.background3
            , width (pct 100)
            ]
        ]
        [ model
            |> Model.indexedPartNames
            |> List.map (partOptionView partsModel.selectedPartIndex)
            |> Grid.container []
        ]


partOptionView : Maybe Int -> ( Int, String ) -> Html Msg
partOptionView selectedIndex ( index, name ) =
    let
        style : List Style
        style =
            [ Style.hfnss
            , marginLeft (px 10)
            , cursor pointer
            , width (pct 100)
            , hover [ highlight ]
            , Util.styleIf
                (selectedIndex == Just index)
                highlight
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


highlight : Style
highlight =
    [ backgroundColor Colors.background4
    , color Colors.point1
    ]
        |> Css.batch
