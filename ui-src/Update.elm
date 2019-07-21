module Update exposing (update)

import Data.Page as Page exposing (Page)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page.Package as Package
import Page.Parts as Parts
import Page.Terminal as Terminal
import Service.Api.Play as Play
import Ui.Header as Header
import Ui.Modal as Modal
import Ui.Tracker as Tracker
import Util.Cmd as CmdUtil


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDecodeFailed error ->
            model
                |> Model.setError error
                |> CmdUtil.withNoCmd

        TrackerMsg ti subMsg ->
            case Model.getTrackersPartIndex ti model of
                Just pi ->
                    model
                        |> Tracker.update ti pi subMsg
                        |> CmdUtil.mapCmd (TrackerMsg ti)

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        HeaderMsg subMsg ->
            model
                |> Header.update subMsg
                |> CmdUtil.mapCmd HeaderMsg

        PackageMsg subMsg ->
            model
                |> Package.update subMsg
                |> CmdUtil.withNoCmd

        PartsMsg subMsg ->
            case model.page of
                Page.Parts partsModel ->
                    model
                        |> Parts.update partsModel subMsg

                _ ->
                    model
                        |> CmdUtil.withNoCmd

        TerminalMsg subMsg ->
            case model.page of
                Page.Terminal ->
                    Terminal.update subMsg model
                        |> CmdUtil.mapCmd TerminalMsg

                _ ->
                    model
                        |> CmdUtil.withNoCmd

        ModalMsg subMsg ->
            model
                |> Modal.update subMsg
                |> CmdUtil.mapCmd ModalMsg

        EscapePressed ->
            model
                |> Model.clearModal
                |> CmdUtil.withNoCmd

        CmdEnterPressed ->
            onCmdEnterPressed model

        PlayMsg subMsg ->
            Play.update subMsg model
                |> CmdUtil.mapCmd PlayMsg


onCmdEnterPressed : Model -> ( Model, Cmd Msg )
onCmdEnterPressed model =
    case model.page of
        Page.Terminal ->
            Terminal.onCmdEnterPressed model
                |> CmdUtil.mapCmd TerminalMsg

        _ ->
            Play.attempt model
                |> CmdUtil.mapCmd PlayMsg
