module Ui.Header exposing
    ( Msg
    , update
    , view
    )

import Css exposing (..)
import Data.BackendStatus as BackendStatus
import Data.Modal as Modal
import Data.Page as Page exposing (Page)
import Data.Route as Route exposing (Route)
import Data.Size as Size
import Data.Width as Width
import Html.Grid as Grid
import Html.Styled exposing (Html)
import Model exposing (Model)
import Service.Api.Play as Play
import Style
import Util.Cmd as CmdUtil
import View.Button as Button
import View.Card as Card
import View.Checkbox as Checkbox
import View.Input as Input
import View.Text as Text



-- TYPES --


type Msg
    = RouteClicked Route
    | NewTrackerClicked
    | PlayClicked
    | SaveClicked
    | BuildClicked
    | PlayFromFieldUpdated String
    | PlayForFieldUpdated String
    | RepeatClicked
    | PlayMsg Play.Msg



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteClicked route ->
            model
                |> Model.setPage (Route.toPage route)
                |> CmdUtil.withNoCmd

        NewTrackerClicked ->
            model
                |> Model.addNewTracker
                |> CmdUtil.withNoCmd

        PlayClicked ->
            Play.attempt model
                |> CmdUtil.mapCmd PlayMsg

        SaveClicked ->
            model
                |> Model.save
                |> CmdUtil.withModel model

        BuildClicked ->
            model
                |> Model.setModal Modal.initBuild
                |> CmdUtil.withNoCmd

        PlayFromFieldUpdated "" ->
            model
                |> Model.setPlayFrom 0
                |> CmdUtil.withNoCmd

        PlayFromFieldUpdated str ->
            case String.toInt str of
                Just playFrom ->
                    model
                        |> Model.setPlayFrom playFrom
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        PlayForFieldUpdated "" ->
            model
                |> Model.setPlayFor 0
                |> CmdUtil.withNoCmd

        PlayForFieldUpdated str ->
            case String.toInt str of
                Just playFor ->
                    model
                        |> Model.setPlayFor playFor
                        |> CmdUtil.withNoCmd

                Nothing ->
                    model
                        |> CmdUtil.withNoCmd

        RepeatClicked ->
            model
                |> Model.toggleRepeatPlayback
                |> CmdUtil.withNoCmd

        PlayMsg subMsg ->
            Play.update subMsg model
                |> CmdUtil.mapCmd PlayMsg



-- VIEW --


view : Model -> Html Msg
view model =
    Card.config
        [ displayFlex
        , minHeight minContent
        , flexDirection Css.column
        ]
        [ Grid.row [] (playbackButtons model)
        , Grid.row [] (uiButtons model)
        ]


playbackButtons : Model -> List (Grid.Column Msg)
playbackButtons model =
    [ buttonColumn SaveClicked "save"
    , buttonColumn PlayClicked "play"
    , textColumn "from"
    , inputColumn PlayFromFieldUpdated <|
        String.fromInt model.playFromBeat
    , textColumn "for"
    , inputColumn PlayForFieldUpdated <|
        String.fromInt model.playForBeats
    , Grid.column
        [ flex none
        , padding (px 1)
        , Style.width Size.big Width.double
        ]
        [ text "repeat"
        ]
    , Checkbox.checkbox RepeatClicked model.repeatPlayback
        |> Checkbox.toHtml
        |> column
    , buttonColumn BuildClicked "build"
    , Grid.column
        [ alignItems flexEnd
        , flexDirection Css.column
        ]
        [ Grid.box
            [ backgroundColor <|
                BackendStatus.toColor
                    model.backendStatus
            , Style.indent
            , margin (px 1)
            , Style.singleWidth Size.Big
            , height (pct 100)
            ]
            []
        ]
    ]


uiButtons : Model -> List (Grid.Column Msg)
uiButtons model =
    [ textColumn "page"
    , pageButtonColumn Route.Trackers model.page
    , pageButtonColumn Route.Package model.page
    , pageButtonColumn Route.Parts model.page
    , pageButtonColumn Route.Terminal model.page
    ]
        ++ newTrackerButton model.page


newTrackerButton : Page -> List (Grid.Column Msg)
newTrackerButton page =
    case page of
        Page.Trackers ->
            [ horizontalSeparator
            , Button.config NewTrackerClicked "new tracker"
                |> Button.withWidth Width.double
                |> Button.makeTallerBy 4
                |> Button.toHtml
                |> column
            ]

        _ ->
            []


column : Html Msg -> Grid.Column Msg
column childButton =
    Grid.column
        [ flex (int 0)
        , padding (px 1)
        ]
        [ childButton ]


text : String -> Html Msg
text =
    Text.withStyles
        [ lineHeight (px 32)
        , Style.singleWidth Size.big
        , textAlign center
        ]


textColumn : String -> Grid.Column Msg
textColumn =
    column << text


horizontalSeparator : Grid.Column Msg
horizontalSeparator =
    Grid.column
        [ flex none
        , Style.singleWidth Size.big
        ]
        []


inputColumn : (String -> Msg) -> String -> Grid.Column Msg
inputColumn msgCtor value =
    Input.config msgCtor value
        |> Input.makeTallerBy 4
        |> Input.toHtml
        |> column


button : Msg -> String -> Html Msg
button msg label =
    Button.config msg label
        |> Button.withWidth Width.single
        |> Button.makeTallerBy 4
        |> Button.toHtml


buttonColumn : Msg -> String -> Grid.Column Msg
buttonColumn msg label =
    column <| button msg label


pageButton : Route -> Page -> Html Msg
pageButton route currentPage =
    Button.config (RouteClicked route) (Route.toString route)
        |> Button.withWidth Width.double
        |> Button.makeTallerBy 4
        |> Button.indent (Route.isPage currentPage route)
        |> Button.toHtml


pageButtonColumn : Route -> Page -> Grid.Column Msg
pageButtonColumn route currentPage =
    column <| pageButton route currentPage
