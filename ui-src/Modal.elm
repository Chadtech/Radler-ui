module Modal exposing
    ( Msg
    , update
    , view
    )

import Css exposing (..)
import Data.Modal exposing (Modal(..))
import Error
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Modal.Build
import Model exposing (Model)
import Return2 as R2
import Style



-- TYPES --


type Msg
    = ErrorMsg Error.Msg
    | BuildMsg Modal.Build.Msg



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErrorMsg subMsg ->
            model
                |> Error.update subMsg
                |> R2.withNoCmd

        BuildMsg subMsg ->
            case model.modal of
                Just (BuildConfirmation buildModel) ->
                    model
                        |> Modal.Build.update subMsg buildModel
                        |> R2.mapCmd BuildMsg

                _ ->
                    model
                        |> R2.withNoCmd



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
            , marginTop (pct 50)
            , marginLeft (pct 50)
            , transform (translate2 (pct -50) (pct -50))
            , maxHeight (px 600)
            , overflow scroll
            ]
        ]
        (modalContent modal)


modalContent : Modal -> List (Html Msg)
modalContent modal =
    case Debug.log "MODAL" modal of
        Error error ->
            [ Error.modalView error
                |> Html.map ErrorMsg
            ]

        BuildConfirmation model ->
            [ model
                |> Modal.Build.view
                |> Html.map BuildMsg
            ]
