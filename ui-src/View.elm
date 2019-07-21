module View exposing (view)

import Css exposing (..)
import Data.Page as Page
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page.Package as Package
import Page.Parts as Parts
import Page.Terminal as Terminal
import Page.Trackers as Trackers
import Style
import Ui.Header as Header
import Ui.Modal as Modal


view : Model -> List (Html Msg)
view model =
    [ Header.view model
        |> Html.map HeaderMsg
    , Grid.row
        [ height (calc (vh 100) minus (px 78))
        , Style.card
        , overflow hidden
        ]
        (body model)
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


body : Model -> List (Grid.Column Msg)
body model =
    case model.page of
        Page.Package ->
            Package.view model
                |> mapColumns PackageMsg

        Page.Trackers ->
            Trackers.view model

        Page.Parts partsModel ->
            Parts.view
                model
                partsModel
                |> mapColumns PartsMsg

        Page.Terminal ->
            Terminal.view model
                |> mapColumns TerminalMsg


mapColumns : (a -> b) -> List (Grid.Column a) -> List (Grid.Column b)
mapColumns =
    List.map << Grid.mapColumn
