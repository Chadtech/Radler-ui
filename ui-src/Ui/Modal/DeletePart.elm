module Ui.Modal.DeletePart exposing
    ( Msg
    , msgDecoderFromType
    , update
    , view
    )

import Css exposing (..)
import Data.Index as Index exposing (Index)
import Data.Modal.DeletePart as DeletePart
import Data.Part as Part exposing (Part)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Model exposing (Model)
import Ports
import Util.Cmd as CmdUtil
import View.Button as Button



-- TYPES --


type Msg
    = YesClicked
    | CancelClicked
    | PartDeleted (Result String (Index Part))



-- UPDATE --


update : Msg -> DeletePart.Model -> Model -> ( Model, Cmd Msg )
update msg deletePartModel model =
    case msg of
        YesClicked ->
            let
                getPartToDelete : Index Part -> Maybe ( Index Part, String )
                getPartToDelete index =
                    Model.getPart index model
                        |> Maybe.map .name
                        |> Maybe.map (Tuple.pair index)
            in
            case
                deletePartModel
                    |> DeletePart.getPartIndex
                    |> Maybe.andThen getPartToDelete
            of
                Just ( index, partName ) ->
                    ( Model.setDeletePartModal DeletePart.Deleting model
                    , Ports.DeletePartFromDisk partName index
                        |> Ports.send
                    )

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        CancelClicked ->
            model
                |> Model.clearModal
                |> CmdUtil.withNoCmd

        PartDeleted (Ok int) ->
            model
                |> Model.deletePart int
                |> Model.clearModal
                |> Model.clearPartsPage
                |> CmdUtil.withNoCmd

        PartDeleted (Err err) ->
            Model.setDeletePartModal
                (DeletePart.Error err)
                model
                |> CmdUtil.withNoCmd



-- VIEW --


view : DeletePart.Model -> Model -> Html Msg
view deletePartModel model =
    Grid.container
        [ maxWidth (px 375) ]
        (contentView deletePartModel model)


contentView : DeletePart.Model -> Model -> List (Html Msg)
contentView deletePartModel model =
    case deletePartModel of
        DeletePart.Ready partIndex ->
            case Model.getPart partIndex model of
                Just part ->
                    readyView part

                Nothing ->
                    [ Html.p
                        []
                        [ Html.text "error! part not found" ]
                    ]

        DeletePart.Deleting ->
            [ Html.p
                []
                [ Html.text "deleting.." ]
            ]

        DeletePart.Error error ->
            [ Html.p
                []
                [ Html.text "error : "
                , Html.text error
                ]
            ]


readyView : Part -> List (Html Msg)
readyView part =
    let
        message : Html Msg
        message =
            Html.p
                []
                [ [ "Are you sure you want to delete "
                  , part.name
                  , "?"
                  ]
                    |> String.concat
                    |> Html.text
                ]
    in
    [ Grid.row
        [ marginBottom (px 5) ]
        [ Grid.column
            []
            [ message ]
        ]
    , Grid.row
        [ justifyContent center ]
        [ Grid.column
            [ flex (int 0) ]
            [ Button.button YesClicked "yes"
                |> Button.withWidth Button.singleWidth
                |> Button.toHtml
            ]
        , Grid.column
            [ flex (int 0)
            , marginLeft (px 5)
            ]
            [ Button.button CancelClicked "cancel"
                |> Button.withWidth Button.singleWidth
                |> Button.toHtml
            ]
        ]
    ]



-- PORTS --


msgDecoderFromType : String -> Decoder Msg
msgDecoderFromType type_ =
    case type_ of
        "partDeleted" ->
            [ Decode.map Ok Index.decoder
            , Decode.map Err Decode.string
            ]
                |> Decode.oneOf
                |> Decode.map PartDeleted

        _ ->
            ("Not a Delete Part Msg -> " ++ type_)
                |> Decode.fail
