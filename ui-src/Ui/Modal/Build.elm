module Ui.Modal.Build exposing
    ( Msg
    , tests
    , update
    , view
    )

import Api
import BackendStatus as BackendStatus
import Css exposing (..)
import Data.Error as Error
import Data.Modal.Build as Build
import Expect
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Model exposing (Model)
import Service.Api as Api
import Style
import Test exposing (Test, describe, test)
import Util.Cmd as CmdUtil
import View.Button as Button



-- TYPES --


type Msg
    = BuildClicked
    | CancelClicked
    | GoBackClicked
    | GotBuildResponse (Result Api.Error ())



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildClicked ->
            handleBuildClicked model

        CancelClicked ->
            closeModal model

        GoBackClicked ->
            closeModal model

        GotBuildResponse response ->
            let
                idleModel : Model
                idleModel =
                    Model.setBackendStatusIdle model
            in
            case response of
                Ok () ->
                    idleModel
                        |> Model.setBuildModal Build.Finished
                        |> CmdUtil.withNoCmd

                Err error ->
                    idleModel
                        |> Model.setError (Error.ApiError error)
                        |> CmdUtil.withNoCmd


closeModal : Model -> ( Model, Cmd Msg )
closeModal =
    Model.clearModal >> CmdUtil.withNoCmd


handleBuildClicked : Model -> ( Model, Cmd Msg )
handleBuildClicked model =
    case Model.fullScore model of
        Ok scoreStr ->
            Api.sendBuild
                { score = scoreStr
                , handler = GotBuildResponse
                }
                model

        Err newModel ->
            newModel
                |> CmdUtil.withNoCmd



-- VIEW --


view : Build.Model -> Html Msg
view buildModel =
    Grid.box
        [ maxWidth (px 375) ]
        (viewContent buildModel)


viewContent : Build.Model -> List (Html Msg)
viewContent buildModel =
    case buildModel of
        Build.Ready ->
            readyView

        Build.Building ->
            buildView

        Build.Finished ->
            finishedView


finishedView : List (Html Msg)
finishedView =
    [ Grid.row
        [ marginBottom (px 5) ]
        [ Grid.column
            []
            [ text "done building" ]
        ]
    , Grid.row
        [ justifyContent center ]
        [ Grid.column
            [ flex (int 0) ]
            [ button GoBackClicked "go back" ]
        ]
    ]


buildView : List (Html Msg)
buildView =
    [ Grid.row
        []
        [ Grid.column
            []
            [ text "building.." ]
        ]
    ]


readyView : List (Html Msg)
readyView =
    [ Grid.row
        [ marginBottom (px 5) ]
        [ Grid.column
            []
            [ text "building takes a long time, are you sure you want to build this piece?" ]
        ]
    , Grid.row
        [ justifyContent center ]
        [ Grid.column
            [ flex (int 0)
            ]
            [ button BuildClicked "build" ]
        , Grid.column
            [ flex (int 0)
            , marginLeft (px 10)
            ]
            [ button CancelClicked "cancel" ]
        ]
    ]


text : String -> Html msg
text str =
    Html.p
        [ Attrs.css [ Style.hfnss ] ]
        [ Html.text str ]


button : Msg -> String -> Html Msg
button clickMsg label =
    Button.config clickMsg label
        |> Button.withWidth Button.singleWidth
        |> Button.toHtml



-- TESTS --


tests : Model -> Test
tests testModel =
    describe "Ui.Modal.Build"
        [ test "BuildClick leads to the backend status being Working" <|
            \_ ->
                update BuildClicked testModel
                    |> Tuple.first
                    |> .backendStatus
                    |> Expect.equal BackendStatus.Working
        , test "BuildSent leads to the backend status being Idle, under ideal conditions" <|
            \_ ->
                update (GotBuildResponse <| Ok ()) testModel
                    |> Tuple.first
                    |> .backendStatus
                    |> Expect.equal BackendStatus.Idle
        ]
