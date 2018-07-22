module View exposing (view)

import Array
import Browser
import Colors
import Css exposing (..)
import Data.Tracker exposing (Tracker)
import Header
import Html.Custom exposing (p)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Json.Decode as D
import Model exposing (Model)
import Msg exposing (Msg(..))
import Package
import Style
import Tracker


-- VIEW --


view : Result D.Error Model -> Browser.Document Msg
view result =
    case result of
        Ok model ->
            { title = "Radler"
            , body =
                [ Header.view model
                    |> Html.map HeaderMsg
                , body model
                ]
                    |> List.map Html.toUnstyled
            }

        Err err ->
            { title = "Error"
            , body =
                [ errorView err
                    |> Html.toUnstyled
                ]
            }



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
        [ height (calc (vh 100) minus (px 56)) ]
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
    div
        [ css
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



-- ERROR VIEW --


errorView : D.Error -> Html Msg
errorView error =
    Grid.row
        [ flex (int 1) ]
        [ Grid.column
            [ Style.card
            , Style.basicSpacing
            , overflow hidden
            ]
            [ p
                [ css [ Style.hfnss ] ]
                [ Html.text (D.errorToString error) ]
            ]
        ]
