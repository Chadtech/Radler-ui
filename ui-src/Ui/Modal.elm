module Ui.Modal exposing
    ( Msg
    , msgDecoderFromType
    , update
    , view
    )

import Css exposing (..)
import Data.Modal exposing (Modal(..))
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Model exposing (Model)
import Style
import Ui.Error as Error
import Ui.Modal.Build as Build
import Ui.Modal.DeletePart as DeletePart
import Util.Cmd as CmdUtil
import View.Card as Card



-- TYPES --


type Msg
    = ErrorMsg Error.Msg
    | BuildMsg Build.Msg
    | DeletePartMsg DeletePart.Msg



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErrorMsg subMsg ->
            model
                |> Error.update subMsg
                |> CmdUtil.withNoCmd

        BuildMsg subMsg ->
            model
                |> Build.update subMsg
                |> CmdUtil.mapCmd BuildMsg

        DeletePartMsg subMsg ->
            case model.modal of
                Just (DeletePart deletePartModel) ->
                    model
                        |> DeletePart.update subMsg deletePartModel
                        |> CmdUtil.mapCmd DeletePartMsg

                _ ->
                    model
                        |> CmdUtil.withNoCmd



-- VIEW --


view : Model -> Modal -> Html Msg
view model modal =
    modalCard model modal
        |> backdrop


backdrop : Html Msg -> Html Msg
backdrop child =
    Grid.box
        [ position absolute
        , left (px 0)
        , top (px 0)
        , right (px 0)
        , bottom (px 0)
        , Style.dim
        , zIndex (int 2)
        ]
        [ child ]


modalCard : Model -> Modal -> Html Msg
modalCard model modal =
    Card.config
        [ Style.bigSpacing
        , marginTop (vh 50)
        , marginLeft (pct 50)
        , transform (translate2 (pct -50) (pct -50))
        , maxHeight (px 600)
        , overflow scroll
        ]
        (modalContent model modal)


modalContent : Model -> Modal -> List (Html Msg)
modalContent model modal =
    case modal of
        Error error ->
            [ Error.modalView error
                |> Html.map ErrorMsg
            ]

        BuildConfirmation buildModel ->
            [ buildModel
                |> Build.view
                |> Html.map BuildMsg
            ]

        DeletePart deleteModel ->
            [ DeletePart.view
                deleteModel
                model
                |> Html.map DeletePartMsg
            ]



-- PORTS --


msgDecoderFromType : String -> Decoder Msg
msgDecoderFromType type_ =
    [ DeletePart.msgDecoderFromType type_
        |> Decode.map DeletePartMsg
    ]
        |> Decode.oneOf
