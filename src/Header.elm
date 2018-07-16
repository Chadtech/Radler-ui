module Header
    exposing
        ( Msg
        , update
        , view
        )

import Array
import Colors
import Css exposing (..)
import Data.Sheet as Sheet
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
    | NewSheetClicked



-- UPDATE --


update : Msg -> Model -> Model
update msg model =
    case msg of
        PageClicked page ->
            { model | page = page }

        NewSheetClicked ->
            { model
                | sheets =
                    Array.push Sheet.empty model.sheets
            }



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
                , dent model.page Model.Trackers
                ]
            , onClick (PageClicked Model.Trackers)
            ]
            [ Html.text "trackers" ]
        , button
            [ css
                [ buttonStyle
                , dent model.page Model.Package
                ]
            , onClick (PageClicked Model.Package)
            ]
            [ Html.text "package" ]
        , button
            [ css
                [ buttonStyle
                , marginLeft (px 10)
                ]
            , onClick NewSheetClicked
            ]
            [ Html.text "new sheet" ]
        ]


buttonStyle : Style
buttonStyle =
    [ Style.hfnss
    , margin (px 1)
    , width (px (Style.cellWidth Style.Big * 1.5))
    , height (px (Style.cellHeight Style.Big))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , outline none
    , active [ Style.indent ]
    , Style.outdent
    ]
        |> Css.batch


dent : Page -> Page -> Style
dent currentPage thisPage =
    if currentPage == thisPage then
        Style.indent
    else
        Style.outdent
