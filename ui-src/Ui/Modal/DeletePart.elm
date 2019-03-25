module Ui.Modal.DeletePart exposing
    ( Msg
    , partDeleted
    , update
    , view
    )

import Css exposing (..)
import Data.Modal.DeletePart as DeletePart
import Data.Part as Part exposing (Part)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Ports
import Style
import Util.Cmd as CmdUtil



-- TYPES --


type Msg
    = YesClicked
    | CancelClicked
    | PartDeleted (Result String Int)


partDeleted : Result String Int -> Msg
partDeleted =
    PartDeleted



-- UPDATE --


update : Msg -> DeletePart.Model -> Model -> ( Model, Cmd Msg )
update msg deletePartModel model =
    case msg of
        YesClicked ->
            let
                getPartToDelete : Int -> Maybe ( Int, String )
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
        DeletePart.Ready { partIndex } ->
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

        yesButton : Html Msg
        yesButton =
            Html.button
                [ Attrs.css
                    [ Style.clickableButtonStyle Style.Big
                    , width (px 125)
                    ]
                , Events.onClick YesClicked
                ]
                [ Html.text "yes" ]

        cancelButton : Html Msg
        cancelButton =
            Html.button
                [ Attrs.css
                    [ Style.clickableButtonStyle Style.Big
                    , width (px 125)
                    , marginLeft (px 5)
                    ]
                , Events.onClick CancelClicked
                ]
                [ Html.text "cancel" ]
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
            [ yesButton ]
        , Grid.column
            [ flex (int 0) ]
            [ cancelButton ]
        ]
    ]
