module Modal.Build exposing
    ( Msg
    , update
    , view
    )

import Css exposing (..)
import Data.Error as Error
import Data.Modal.Build as Build
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Model exposing (Model)
import Return2 as R2
import Score
import Style



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
                |> R2.withNoCmd

        BuildSent (Err err) ->
            buildFailed err model
                |> R2.withNoCmd


closeModal : Model -> ( Model, Cmd Msg )
closeModal =
    Model.clearModal >> R2.withNoCmd


handleBuildClicked : Build.Model -> Model -> ( Model, Cmd Msg )
handleBuildClicked buildModel model =
    case buildModel of
        Build.Ready ->
            case Model.fullScore model of
                Ok scoreStr ->
                    ( Model.setBuildModal
                        Build.Building
                        model
                    , scoreStr
                        |> Build
                        |> sendHttp model
                    )

                Err newModel ->
                    newModel
                        |> R2.withNoCmd

        _ ->
            model
                |> R2.withNoCmd


buildFailed : Http.Error -> Model -> Model
buildFailed error =
    error
        |> Score.errorToString
        |> Error.BackendHadProblemWithScore
        |> Model.setError



-- HTTP --


type Call
    = Build String


sendHttp : Model -> Call -> Cmd Msg
sendHttp model call =
    case call of
        Build score ->
            { model = model
            , path = "build"
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
