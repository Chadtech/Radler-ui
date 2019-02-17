module Ui.Modal.Build exposing
    ( Msg
    , tests
    , update
    , view
    )

import BackendStatus as BackendStatus
import Css exposing (..)
import Data.Error as Error
import Data.Modal.Build as Build
import Expect exposing (Expectation)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Model exposing (Model)
import Score
import Style
import Test exposing (Test, describe, test)
import Util



-- TYPES --


type Msg
    = BuildClicked
    | CancelClicked
    | GoBackClicked
    | BuildSent (Result Http.Error ())



-- UPDATE --


update : Msg -> Build.Model -> Model -> ( Model, Cmd Msg )
update msg buildModel model =
    case msg of
        BuildClicked ->
            handleBuildClicked buildModel model

        CancelClicked ->
            closeModal model

        GoBackClicked ->
            closeModal model

        BuildSent (Ok ()) ->
            model
                |> Model.setBuildModal Build.Finished
                |> Model.setBackendStatusIdle
                |> Util.withNoCmd

        BuildSent (Err err) ->
            model
                |> Model.setBackendStatusIdle
                |> buildFailed err
                |> Util.withNoCmd


closeModal : Model -> ( Model, Cmd Msg )
closeModal =
    Model.clearModal >> Util.withNoCmd


handleBuildClicked : Build.Model -> Model -> ( Model, Cmd Msg )
handleBuildClicked buildModel model =
    case buildModel of
        Build.Ready ->
            case Model.fullScore model of
                Ok scoreStr ->
                    ( model
                        |> Model.setBuildModal Build.Building
                        |> Model.setBackendStatusWorking
                    , scoreStr
                        |> Build
                        |> sendHttp
                    )

                Err newModel ->
                    newModel
                        |> Util.withNoCmd

        _ ->
            model
                |> Util.withNoCmd


buildFailed : Http.Error -> Model -> Model
buildFailed error =
    error
        |> Score.errorToString
        |> Error.BackendHadProblemWithScore
        |> Model.setError



-- HTTP --


type Call
    = Build String


sendHttp : Call -> Cmd Msg
sendHttp call =
    case call of
        Build score ->
            { path = "build"
            , score = score
            , msgCtor = BuildSent
            }
                |> Score.sendHttp



-- VIEW --


view : Build.Model -> Html Msg
view buildModel =
    Grid.container
        [ maxWidth (px 300) ]
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
            [ Html.p
                [ Attrs.css [ Style.hfnss ] ]
                [ Html.text "done building" ]
            ]
        ]
    , Grid.row
        [ justifyContent center ]
        [ Grid.column
            [ flex (int 0) ]
            [ goBackButton ]
        ]
    ]


buildView : List (Html Msg)
buildView =
    [ Grid.row
        []
        [ Grid.column
            []
            [ Html.p
                [ Attrs.css [ Style.hfnss ] ]
                [ Html.text "building.." ]
            ]
        ]
    ]


readyView : List (Html Msg)
readyView =
    [ Grid.row
        [ marginBottom (px 5) ]
        [ Grid.column
            []
            [ Html.p
                [ Attrs.css
                    [ Style.hfnss ]
                ]
                [ Html.text "building takes a long time, are you sure you want to build this piece?" ]
            ]
        ]
    , Grid.row
        [ justifyContent center ]
        [ Grid.column
            [ flex (int 0)
            ]
            [ buildButton ]
        , Grid.column
            [ flex (int 0)
            , marginLeft (px 10)
            ]
            [ cancelButton ]
        ]
    ]


goBackButton : Html Msg
goBackButton =
    button GoBackClicked "go back"


buildButton : Html Msg
buildButton =
    button BuildClicked "build"


cancelButton : Html Msg
cancelButton =
    button CancelClicked "cancel"


button : Msg -> String -> Html Msg
button clickMsg label =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick clickMsg
        ]
        [ Html.text label ]


buttonStyle : Style
buttonStyle =
    [ Style.basicButton Style.Big
    , width (px (Style.noteWidth Style.Big))
    , active [ Style.indent ]
    ]
        |> Css.batch



-- TESTS --


tests : Model -> Test
tests testModel =
    describe "Ui.Modal.Build"
        [ test "BuildClick leads to the backend status being Working" <|
            \_ ->
                update BuildClicked Build.Ready testModel
                    |> Tuple.first
                    |> .backendStatus
                    |> Expect.equal BackendStatus.Working
        , test "BuildSent leads to the backend status being Idle, under ideal conditions" <|
            \_ ->
                update (BuildSent <| Ok ()) Build.Ready testModel
                    |> Tuple.first
                    |> .backendStatus
                    |> Expect.equal BackendStatus.Idle
        ]
