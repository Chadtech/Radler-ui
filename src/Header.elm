module Header
    exposing
        ( Msg
        , update
        , view
        )

import Colors
import Css exposing (..)
import Data.Tracker as Tracker
import Html.Grid as Grid
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
import Model exposing (Model, Page)
import Return2 as R2
import Style


-- TYPES --


type Msg
    = PageClicked Page



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageClicked page ->
            { model | page = page }
                |> R2.withNoCmd



-- VIEW --


view : Model -> Html Msg
view model =
    Grid.row
        [ Style.card
        , displayFlex
        , minHeight minContent
        ]
        [ button
            [ css
                [ buttonStyle
                    model.page
                    Model.Trackers
                ]
            , onClick (PageClicked Model.Trackers)
            ]
            [ Html.text "Trackers" ]
        , button
            [ css
                [ buttonStyle
                    model.page
                    Model.Package
                ]
            , onClick (PageClicked Model.Package)
            ]
            [ Html.text "Package" ]
        ]


buttonStyle : Page -> Page -> Style
buttonStyle currentPage thisPage =
    [ dent currentPage thisPage
    , Style.hfnss
    , margin (px 1)
    , width (px (Style.cellWidth Style.Big * 1.5))
    , height (px (Style.cellHeight Style.Big))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , outline none
    ]
        |> Css.batch


dent : Page -> Page -> Style
dent currentPage thisPage =
    if currentPage == thisPage then
        Style.indent
    else
        Style.outdent
