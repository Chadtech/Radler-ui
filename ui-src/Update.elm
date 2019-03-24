module Update exposing (update)

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
        MsgDecodeFailed _ ->
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
            model
                |> Parts.update subMsg
                |> Util.withNoCmd

        ModalMsg subMsg ->
            model
                |> Modal.update subMsg
                |> Util.mapCmd ModalMsg
