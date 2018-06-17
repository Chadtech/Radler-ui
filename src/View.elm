module View exposing (view)

import Browser
import Model exposing (Model)
import Msg exposing (Msg(..))
import Tracker.View as Tracker


-- VIEW --


view : Model -> Browser.Page Msg
view model =
    { title = "Radler"
    , body =
        model.trackers
            |> Array.toIndexedList
            |> List.map (Tracker.view model)
    }
