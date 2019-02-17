module View exposing (view)

import Array
import Colors
import Css exposing (..)
import Data.Tracker exposing (Tracker)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style
import Ui.Header as Header
import Ui.Modal as Modal
import Ui.Package as Package
import Ui.Tracker as Tracker


view : Model -> List (Html Msg)
view model =
    [ Header.view model
        |> Html.map HeaderMsg
    , body model
    , modalView model
    ]



-- MODAL --


modalView : Model -> Html Msg
modalView model =
    case model.modal of
        Nothing ->
            Html.text ""

        Just modal ->
            modal
                |> Modal.view
                |> Html.map ModalMsg



-- BODY --


body : Model -> Html Msg
body model =
    case model.page of
        Model.Package ->
            packageContainer model

        Model.Trackers ->
            trackersContainer model


packageContainer : Model -> Html Msg
packageContainer model =
    Grid.row
        [ height (calc (vh 100) minus (px 73)) ]
        [ Grid.column
            [ Style.card
            , Style.basicSpacing
            , overflow hidden
            ]
            [ Package.view model
                |> Html.map PackageMsg
            ]
        ]


trackersContainer : Model -> Html Msg
trackersContainer model =
    Grid.row
        [ flex (int 1) ]
        [ Grid.column
            [ Style.card
            , Style.basicSpacing
            , overflow hidden
            ]
            [ trackersBody model ]
        ]


trackersBody : Model -> Html Msg
trackersBody model =
    Html.div
        [ Attrs.css
            [ Style.indent
            , width (pct 100)
            , Style.basicSpacing
            , backgroundColor Colors.background1
            , overflow auto
            ]
        ]
        [ Grid.container
            [ display inlineFlex
            , Style.basicSpacing
            ]
            (viewTrackers model)
        ]


viewTrackers : Model -> List (Html Msg)
viewTrackers model =
    model.trackers
        |> Array.toIndexedList
        |> List.map (viewTracker model)


viewTracker : Model -> ( Int, Tracker ) -> Html Msg
viewTracker model ( trackerIndex, tracker ) =
    tracker
        |> Tracker.view model trackerIndex
        |> Html.map (TrackerMsg trackerIndex)
