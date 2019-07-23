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
import Data.Width as Width
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Model exposing (Model)
import Ports
import Util.Cmd as CmdUtil
import View.Button as Button
import View.Text as Text



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
    Grid.box
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
                    [ Text.fromString "error! part not found" ]

        DeletePart.Deleting ->
            [ Text.fromString "deleting.." ]

        DeletePart.Error error ->
            [ Text.fromString ("error : " ++ error) ]


readyView : Part -> List (Html Msg)
readyView part =
    [ Grid.row
        [ marginBottom (px 5) ]
        [ Grid.column
            []
            [ Text.concat
                [ "Are you sure you want to delete "
                , part.name
                , "?"
                ]
            ]
        ]
    , Grid.row
        [ justifyContent center ]
        [ Grid.column
            [ flex (int 0) ]
            [ Button.config YesClicked "yes"
                |> Button.withWidth Width.single
                |> Button.toHtml
            ]
        , Grid.column
            [ flex (int 0)
            , marginLeft (px 5)
            ]
            [ Button.config CancelClicked "cancel"
                |> Button.withWidth Width.single
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
