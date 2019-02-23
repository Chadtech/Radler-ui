module Ui.Modal exposing
    ( Msg
    , update
    , view
    )

import Css exposing (..)
import Data.Modal exposing (Modal(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Model exposing (Model)
import Style
import Ui.Error as Error
import Ui.Modal.Build as Build
import Util



-- TYPES --


type Msg
    = ErrorMsg Error.Msg
    | BuildMsg Build.Msg



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErrorMsg subMsg ->
            model
                |> Error.update subMsg
                |> Util.withNoCmd

        BuildMsg subMsg ->
            case model.modal of
                Just (BuildConfirmation buildModel) ->
                    model
                        |> Build.update subMsg buildModel
                        |> Util.mapCmd BuildMsg

                _ ->
                    model
                        |> Util.withNoCmd



-- VIEW --


view : Modal -> Html Msg
view =
    modalCard >> backdrop


backdrop : Html Msg -> Html Msg
backdrop child =
    Html.div
        [ Attrs.css
            [ position absolute
            , left (px 0)
            , top (px 0)
            , right (px 0)
            , bottom (px 0)
            , Style.dim
            , zIndex (int 2)
            ]
        ]
        [ child ]


modalCard : Modal -> Html Msg
modalCard modal =
    Html.div
        [ Attrs.css
            [ Style.card
            , Style.bigSpacing
            , marginTop (vh 50)
            , marginLeft (pct 50)
            , transform (translate2 (pct -50) (pct -50))
            , maxHeight (px 600)
            , overflow scroll
            ]
        ]
        (modalContent modal)


modalContent : Modal -> List (Html Msg)
modalContent modal =
    case modal of
        Error error ->
            [ Error.modalView error
                |> Html.map ErrorMsg
            ]

        BuildConfirmation model ->
            [ model
                |> Build.view
                |> Html.map BuildMsg
            ]
