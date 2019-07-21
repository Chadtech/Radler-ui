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
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style
import Util.Cmd as CmdUtil
import Util.Css as CssUtil
import View.Button as Button



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
        [ paddingLeft (px 5) ]
        [ maybeSelectedPartView model maybePartsModel ]
    ]



-- PART VIEW --


maybeSelectedPartView : Model -> Maybe Parts.Model -> Html Msg
maybeSelectedPartView model maybePartsModel =
    let
        nothingView : String -> Html Msg
        nothingView str =
            Html.p
                []
                [ Html.text str ]
    in
    case maybePartsModel of
        Just partsModel ->
            case Model.getPart partsModel.selectedPartIndex model of
                Just part ->
                    partView partsModel part

                Nothing ->
                    nothingView "error! part not found"

        Nothing ->
            nothingView "no part selected"


partView : Parts.Model -> Part -> Html Msg
partView partsModel part =
    Grid.box
        [ width (pct 100) ]
        [ Grid.row
            []
            [ Grid.column
                []
                [ Html.input
                    [ Attrs.css
                        [ color Colors.point0
                        , width (pct 100)
                        , Style.fontSmoothingNone
                        ]
                    , Attrs.value part.name
                    , Attrs.spellcheck False
                    , Events.onInput NameFieldUpdated
                    ]
                    []
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
                    |> Button.withWidth Button.doubleWidth
                    |> Button.toHtml
                ]
            ]
        ]
        |> Html.map SelectedPartMsg


copyWithNameField : Parts.Model -> List (Grid.Column SelectedPartMsg)
copyWithNameField model =
    [ Grid.column
        []
        [ Html.input
            [ Attrs.css
                [ Style.hfnss
                , color Colors.point0
                , Style.fontSmoothingNone
                , width (pct 100)
                ]
            , Events.onInput CopyNameChanged
            , Attrs.value model.copyName
            ]
            []
        ]
    , Grid.column
        [ paddingLeft (px 5)
        , flex (int 0)
        ]
        [ Button.config CopyWithNameClicked "copy with name"
            |> Button.withWidth Button.doubleWidth
            |> Button.toHtml
        ]
    ]



-- PARTS LIST VIEW --


partsView : Model -> Maybe Parts.Model -> Html Msg
partsView model maybePartsModel =
    Grid.box
        [ width (pct 100)
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
                    |> Button.withWidth Button.fullWidth
                    |> Button.toHtml
                ]
            ]
        ]


partsListView : Model -> Maybe Parts.Model -> Html Msg
partsListView model partsModel =
    Html.div
        [ Attrs.css
            [ Style.indent
            , backgroundColor Colors.background3
            , width (pct 100)
            ]
        ]
        [ model
            |> Model.indexedPartNames
            |> List.map
                (partOptionView (Maybe.map .selectedPartIndex partsModel))
            |> Grid.box []
        ]


partOptionView : Maybe (Index Part) -> ( Index Part, String ) -> Html Msg
partOptionView selectedIndex ( index, name ) =
    let
        highlight : Style
        highlight =
            [ backgroundColor Colors.background4
            , color Colors.point1
            ]
                |> Css.batch

        style : List Style
        style =
            [ Style.hfnss
            , marginLeft (px 10)
            , cursor pointer
            , width (pct 100)
            , hover [ highlight ]
            , CssUtil.styleIf
                (selectedIndex == Just index)
                highlight
            ]
    in
    Grid.row
        [ Style.basicSpacing ]
        [ Grid.column
            []
            [ Html.p
                [ Attrs.css style
                , Events.onClick (PartClicked index)
                ]
                [ Html.text name ]
            ]
        ]
