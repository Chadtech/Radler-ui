module Update exposing (update)

import Cell
import Data.Sheet as Sheet
import Header
import Model exposing (Model)
import Msg exposing (Msg(..))
import Return2 as R2
import Row
import Tracker


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDecodeFailed _ ->
            model
                |> R2.withNoCmd

        TrackerMsg ti (Tracker.RowMsg ri (Row.CellMsg ci (Cell.Updated str))) ->
            case Model.getThreadsSheetIndex ti model of
                Just si ->
                    Row.setCell ci str
                        |> Sheet.mapRow ri
                        |> Model.mapSheet si
                        |> (|>) model
                        |> R2.withNoCmd

                Nothing ->
                    model
                        |> R2.withNoCmd

        HeaderMsg subMsg ->
            Header.update subMsg model
                |> R2.mapCmd HeaderMsg
