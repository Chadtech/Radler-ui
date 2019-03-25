module Update exposing (update)

import Data.Page as Page
import Json.Decode as Decode
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page.Package as Package
import Page.Parts as Parts
import Ui.Header as Header
import Ui.Modal as Modal
import Ui.Tracker as Tracker
import Util


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDecodeFailed error ->
            model
                |> Util.withNoCmd

        TrackerMsg ti subMsg ->
            case Model.getTrackersPartIndex ti model of
                Just pi ->
                    model
                        |> Tracker.update ti pi subMsg
                        |> Util.mapCmd (TrackerMsg ti)

                Nothing ->
                    model
                        |> Util.withNoCmd

        HeaderMsg subMsg ->
            model
                |> Header.update subMsg
                |> Util.mapCmd HeaderMsg

        PackageMsg subMsg ->
            model
                |> Package.update subMsg
                |> Util.withNoCmd

        PartsMsg subMsg ->
            case model.page of
                Page.Parts partsModel ->
                    model
                        |> Parts.update partsModel subMsg
                        |> Util.withNoCmd

                _ ->
                    model
                        |> Util.withNoCmd

        ModalMsg subMsg ->
            model
                |> Modal.update subMsg
                |> Util.mapCmd ModalMsg
