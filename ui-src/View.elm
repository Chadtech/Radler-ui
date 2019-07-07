module View exposing (view)

import Css exposing (..)
import Css.Global
import Data.Page as Page
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page.Package as Package
import Page.Parts as Parts
import Page.Trackers as Trackers
import Style
import Ui.Header as Header
import Ui.Modal as Modal


view : Model -> List (Html Msg)
view model =
    [ Css.Global.global
        [ Css.Global.p
            [ Style.hfnss ]
        , Css.Global.input
            [ Style.hfnss ]
        ]
    , Header.view model
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
            Modal.view model modal
                |> Html.map ModalMsg



-- BODY --


body : Model -> Html Msg
body model =
    case model.page of
        Page.Package ->
            Package.view model
                |> Html.map PackageMsg
                |> mainColumn
                |> fullPageCard

        Page.Trackers ->
            Grid.row
                [ flex (int 1) ]
                [ Trackers.view model
                    |> mainColumn
                ]

        Page.Parts partsModel ->
            Parts.view
                model
                partsModel
                |> Html.map PartsMsg
                |> mainColumn
                |> fullPageCard


fullPageCard : Html Msg -> Html Msg
fullPageCard content =
    Grid.row
        [ height (calc (vh 100) minus (px 73)) ]
        [ content ]


mainColumn : Html Msg -> Html Msg
mainColumn content =
    Grid.column
        [ Style.card
        , Style.basicSpacing
        , overflow hidden
        ]
        [ content ]
