module Update exposing (update)

import Cell
import Data.Sheet as Sheet
import Header
import Model exposing (Model)
import Msg exposing (Msg(..))
import Package
import Return2 as R2
import Row
import Tracker


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDecodeFailed _ ->
            model
                |> R2.withNoCmd

        TrackerMsg ti subMsg ->
            case Model.getThreadsSheetIndex ti model of
                Just si ->
                    Tracker.update ti si subMsg model
                        |> R2.mapCmd (TrackerMsg ti)

                Nothing ->
                    model
                        |> R2.withNoCmd

        HeaderMsg subMsg ->
            Header.update subMsg model
                |> R2.withNoCmd

        PackageMsg subMsg ->
            Package.update subMsg model
                |> R2.withNoCmd
