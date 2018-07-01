module View exposing (view)

import Array
import Browser
import Data.Tracker exposing (Tracker)
import Html exposing (Html, div)
import Html.Styled
import Model exposing (Model)
import Msg exposing (Msg(..))
import Tracker.View as Tracker


-- VIEW --


view : Model -> Browser.Document Msg
view model =
    { title = "Radler"
    , body =
        model.trackers
            |> Array.toIndexedList
            |> List.map (viewTracker model)
    }


viewTracker : Model -> ( Int, ( Int, Tracker ) ) -> Html Msg
viewTracker model ( index, rest ) =
    ( index, rest )
        |> Tracker.view model
        |> Html.Styled.toUnstyled
        |> Html.map (TrackerMsg index)
