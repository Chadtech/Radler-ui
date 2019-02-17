module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Return2 as R2
import Ui.Header as Header
import Ui.Modal as Modal
import Ui.Package as Package
import Ui.Tracker as Tracker


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDecodeFailed _ ->
            model
                |> R2.withNoCmd

        TrackerMsg ti subMsg ->
            case Model.getTrackersPartIndex ti model of
                Just pi ->
                    model
                        |> Tracker.update ti pi subMsg
                        |> R2.mapCmd (TrackerMsg ti)

                Nothing ->
                    model
                        |> R2.withNoCmd

        HeaderMsg subMsg ->
            model
                |> Header.update subMsg
                |> R2.mapCmd HeaderMsg

        PackageMsg subMsg ->
            model
                |> Package.update subMsg
                |> R2.withNoCmd

        ModalMsg subMsg ->
            model
                |> Modal.update subMsg
                |> R2.mapCmd ModalMsg
