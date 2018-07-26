module Update exposing (update)

import Html.Header as Header
import Html.Package as Package
import Html.Tracker as Tracker
import Model exposing (Model)
import Msg exposing (Msg(..))
import Return2 as R2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDecodeFailed _ ->
            model
                |> R2.withNoCmd

        TrackerMsg ti subMsg ->
            case Model.getTrackersPartIndex ti model of
                Just si ->
                    Tracker.update ti si subMsg model
                        |> R2.mapCmd (TrackerMsg ti)

                Nothing ->
                    model
                        |> R2.withNoCmd

        HeaderMsg subMsg ->
            Header.update subMsg model
                |> R2.mapCmd HeaderMsg

        PackageMsg subMsg ->
            Package.update subMsg model
                |> R2.withNoCmd
