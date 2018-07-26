module Html.Header
    exposing
        ( Msg
        , update
        , view
        )

import Array
import Colors
import Css exposing (..)
import Data.Part as Part
import Data.Tracker as Tracker
import Html.Custom exposing (p)
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Html
        , button
        , input
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
    | NewTrackerClicked
    | SaveClicked



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageClicked page ->
            { model | page = page }
                |> R2.withNoCmd

        NewSheetClicked ->
            { model
                | parts =
                    Array.push Part.empty model.parts
            }
                |> R2.withNoCmd

        NewTrackerClicked ->
            { model
                | trackers =
                    Array.push
                        (Tracker.init Style.Small 0)
                        model.trackers
            }
                |> R2.withNoCmd

        SaveClicked ->
            [ model
                |> Model.saveToDisk
            ]
                |> Cmd.batch
                |> R2.withModel model



-- VIEW --


view : Model -> Html Msg
view model =
    Grid.row
        [ Style.card
        , displayFlex
        , minHeight minContent
        ]
        [ Grid.column
            [ flex (int 0) ]
            [ saveButton ]
        , Grid.column
            [ flex (int 0)
            , marginLeft (px 10)
            ]
            [ trackersButton model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ packageButton model.page ]
        , Grid.column
            [ marginLeft (px 10)
            , flex (int 0)
            ]
            [ newSheetButton ]
        , Grid.column
            [ flex (int 0) ]
            [ newTrackerButton ]
        ]


saveButton : Html Msg
saveButton =
    button
        [ css [ buttonStyle ]
        , onClick SaveClicked
        ]
        [ Html.text "save" ]


trackersButton : Page -> Html Msg
trackersButton page =
    button
        [ css
            [ buttonStyle
            , dent page Model.Trackers
            ]
        , onClick (PageClicked Model.Trackers)
        ]
        [ Html.text "trackers" ]


packageButton : Page -> Html Msg
packageButton page =
    button
        [ css
            [ buttonStyle
            , dent page Model.Package
            ]
        , onClick (PageClicked Model.Package)
        ]
        [ Html.text "package" ]


newSheetButton : Html Msg
newSheetButton =
    button
        [ css [ buttonStyle ]
        , onClick NewSheetClicked
        ]
        [ Html.text "new part" ]


newTrackerButton : Html Msg
newTrackerButton =
    button
        [ css
            [ buttonStyle
            , width (px (Style.noteWidth Style.Big * 2))
            ]
        , onClick NewTrackerClicked
        ]
        [ Html.text "new tracker" ]


buttonStyle : Style
buttonStyle =
    [ Style.hfnss
    , margin (px 1)
    , width (px (Style.noteWidth Style.Big * 1.5))
    , height (px (Style.noteHeight Style.Big + 4))
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
