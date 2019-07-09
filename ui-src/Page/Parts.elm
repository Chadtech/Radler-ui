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
import Data.Size as Size
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Model exposing (Model)
import Style
import Util.Cmd as CmdUtil
import Util.Css as CssUtil



-- TYPES --


type Msg
    = PartClicked (Index Part)
    | NameFieldUpdated String
    | NewPartClicked
    | SelectedPartMsg SelectedPartMsg


type SelectedPartMsg
    = CopyWithNameClicked
    | CopyNameChanged String
    | DeleteClicked



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

        NameFieldUpdated str ->
            Model.mapSelectedPart
                maybePartsModel
                (Part.setName str)
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



-- VIEW --


view : Model -> Maybe Parts.Model -> Html Msg
view model maybePartsModel =
    Grid.container
        [ margin zero
        , padding (px 5)
        , width (pct 100)
        ]
        [ Grid.row
            [ height (pct 100) ]
            [ Grid.column
                [ flex3 (int 0) (int 1) (px 375) ]
                [ partsView model maybePartsModel ]
            , Grid.column
                [ paddingLeft (px 5) ]
                [ maybeSelectedPartView model maybePartsModel ]
            ]
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
    let
        partNameField : Html Msg
        partNameField =
            let
                style : List Style
                style =
                    [ color Colors.point0
                    , width (pct 100)
                    , Style.fontSmoothingNone
                    ]
            in
            Html.input
                [ Attrs.css style
                , Attrs.value part.name
                , Attrs.spellcheck False
                , Events.onInput NameFieldUpdated
                ]
                []

        deleteButton : Html Msg
        deleteButton =
            Html.button
                [ Attrs.css
                    [ Style.clickableButtonStyle Size.big
                    , width (px 250)
                    ]
                , Events.onClick DeleteClicked
                ]
                [ Html.text "delete part" ]
                |> Html.map SelectedPartMsg
    in
    Grid.container
        [ width (pct 100) ]
        [ Grid.row
            []
            [ Grid.column
                []
                [ partNameField ]
            ]
        , Grid.row
            [ marginTop (px 5) ]
            (copyWithNameField partsModel)
            |> Html.map SelectedPartMsg
        , Grid.row
            [ marginTop (px (5 + 26)) ]
            [ Grid.column
                [ justifyContent flexEnd ]
                [ deleteButton ]
            ]
        ]


copyWithNameField : Parts.Model -> List (Html SelectedPartMsg)
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
        [ Html.button
            [ Attrs.css
                [ Style.clickableButtonStyle Size.big
                , margin (px 0)
                , width (px 250)
                ]
            , Events.onClick CopyWithNameClicked
            ]
            [ Html.text "copy with name" ]
        ]
    ]



-- PARTS LIST VIEW --


partsView : Model -> Maybe Parts.Model -> Html Msg
partsView model maybePartsModel =
    Grid.container
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
                [ newPartButton ]
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
            |> Grid.container []
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


newPartButton : Html Msg
newPartButton =
    Html.button
        [ Attrs.css
            [ Style.clickableButtonStyle Size.big
            , width (pct 100)
            ]
        , Events.onClick NewPartClicked
        ]
        [ Html.text "new part" ]
