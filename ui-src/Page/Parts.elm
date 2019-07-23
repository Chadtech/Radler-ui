module Page.Parts exposing
    ( Msg
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Index exposing (Index)
import Data.Modal as Modal
import Data.Modal.DeletePart as DeletePart
import Data.Page.Parts as Parts
import Data.Part as Part exposing (Part)
import Data.Width as Width
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Model exposing (Model)
import Style
import Util.Cmd as CmdUtil
import Util.Css as CssUtil
import View.Button as Button
import View.Input as Input
import View.Text as Text



-- TYPES --


type Msg
    = PartClicked (Index Part)
    | NewPartClicked
    | SelectedPartMsg SelectedPartMsg


type SelectedPartMsg
    = CopyWithNameClicked
    | CopyNameChanged String
    | DeleteClicked
    | NameFieldUpdated String



-- UPDATE --


update : Maybe Parts.Model -> Msg -> Model -> ( Model, Cmd msg )
update maybePartsModel msg model =
    case msg of
        PartClicked newIndex ->
            case Model.getPart newIndex model of
                Just part ->
                    Model.initPartsPage
                        { copyName = part.name ++ "-copy"
                        , selectedPartIndex = newIndex
                        }
                        model
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        NewPartClicked ->
            Model.addNewPart model

        SelectedPartMsg subMsg ->
            case maybePartsModel of
                Just partsModel ->
                    model
                        |> updatePartModel subMsg partsModel
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd


updatePartModel : SelectedPartMsg -> Parts.Model -> Model -> Model
updatePartModel msg partsModel =
    case msg of
        CopyWithNameClicked ->
            Model.copyPart
                partsModel.selectedPartIndex
                partsModel.copyName

        CopyNameChanged copyName ->
            Parts.setCopyName
                copyName
                partsModel
                |> Model.setPartsPage

        DeleteClicked ->
            partsModel.selectedPartIndex
                |> DeletePart.Ready
                |> Modal.DeletePart
                |> Model.setModal

        NameFieldUpdated str ->
            Model.mapPart
                partsModel.selectedPartIndex
                (Part.setName str)



-- VIEW --


view : Model -> Maybe Parts.Model -> List (Grid.Column Msg)
view model maybePartsModel =
    [ Grid.column
        [ flex3 (int 0) (int 1) (px 375) ]
        [ partsView model maybePartsModel ]
    , Grid.column
        [ Style.leftPadding ]
        [ maybeSelectedPartView model maybePartsModel ]
    ]



-- PART VIEW --


maybeSelectedPartView : Model -> Maybe Parts.Model -> Html Msg
maybeSelectedPartView model maybePartsModel =
    case maybePartsModel of
        Just partsModel ->
            case Model.getPart partsModel.selectedPartIndex model of
                Just part ->
                    partView partsModel part

                Nothing ->
                    Text.fromString "error! part not found"

        Nothing ->
            Text.fromString "no part selected"


partView : Parts.Model -> Part -> Html Msg
partView partsModel part =
    Grid.box
        [ Style.fullWidth ]
        [ Grid.row
            []
            [ Grid.column
                []
                [ Input.config NameFieldUpdated part.name
                    |> Input.withWidth Width.full
                    |> Input.toHtml
                ]
            ]
        , Grid.row
            [ marginTop (px 5) ]
            (copyWithNameField partsModel)
        , Grid.row
            [ marginTop (px (5 + 26)) ]
            [ Grid.column
                [ justifyContent flexEnd ]
                [ Button.config DeleteClicked "delete part"
                    |> Button.withWidth Width.double
                    |> Button.toHtml
                ]
            ]
        ]
        |> Html.map SelectedPartMsg


copyWithNameField : Parts.Model -> List (Grid.Column SelectedPartMsg)
copyWithNameField model =
    [ Grid.column
        []
        [ Input.config CopyNameChanged model.copyName
            |> Input.withWidth Width.full
            |> Input.toHtml
        ]
    , Grid.column
        [ Style.leftPadding
        , Grid.columnShrink
        ]
        [ Button.config CopyWithNameClicked "copy with name"
            |> Button.withWidth Width.double
            |> Button.toHtml
        ]
    ]



-- PARTS LIST VIEW --


partsView : Model -> Maybe Parts.Model -> Html Msg
partsView model maybePartsModel =
    Grid.box
        [ Style.fullWidth
        , displayFlex
        , flexDirection column
        ]
        [ Grid.row
            [ flex (int 1)
            , marginBottom (px 5)
            ]
            [ Grid.column
                []
                [ partsListView model maybePartsModel ]
            ]
        , Grid.row
            []
            [ Grid.column
                []
                [ Button.config NewPartClicked "new part"
                    |> Button.withWidth Width.full
                    |> Button.toHtml
                ]
            ]
        ]


partsListView : Model -> Maybe Parts.Model -> Html Msg
partsListView model partsModel =
    Grid.box
        [ Style.indent
        , backgroundColor Colors.background3
        , Style.fullWidth
        ]
        (model
            |> Model.indexedPartNames
            |> List.map
                (partOptionView (Maybe.map .selectedPartIndex partsModel))
        )


partOptionView : Maybe (Index Part) -> ( Index Part, String ) -> Html Msg
partOptionView selectedIndex ( index, name ) =
    Grid.row
        [ Style.basicSpacing ]
        [ Grid.column
            []
            [ Text.config
                { styles =
                    [ marginLeft (px 10)
                    , cursor pointer
                    , Style.fullWidth
                    , hover [ Style.highlight ]
                    , CssUtil.styleIf
                        (selectedIndex == Just index)
                        Style.highlight
                    ]
                , options = [ Text.onClick (PartClicked index) ]
                , value = name
                }
            ]
        ]
