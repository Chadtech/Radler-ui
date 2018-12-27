module Modal.Build exposing
    ( Msg
    , update
    , view
    )

import Css exposing (..)
import Data.Error as Error
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
    | BuildSent (Result Http.Error ())



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildClicked ->
            case Model.fullScore model of
                Ok scoreStr ->
                    scoreStr
                        |> Build
                        |> sendHttp model
                        |> R2.withModel model

                Err newModel ->
                    newModel
                        |> R2.withNoCmd

        CancelClicked ->
            model
                |> Model.clearModal
                |> R2.withNoCmd

        BuildSent (Ok ()) ->
            model
                |> R2.withNoCmd

        BuildSent (Err err) ->
            buildFailed err model
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


view : Html Msg
view =
    Grid.container
        [ maxWidth (px 300) ]
        [ Grid.row
            [ marginBottom (px 5) ]
            [ Grid.column
                []
                [ Html.p
                    [ Attrs.css
                        [ Style.hfnss ]
                    ]
                    [ Html.text "Building takes a long time, are you sure you want to build this piece?" ]
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


buildButton : Html Msg
buildButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick BuildClicked
        ]
        [ Html.text "build" ]


cancelButton : Html Msg
cancelButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick CancelClicked
        ]
        [ Html.text "cancel" ]


buttonStyle : Style
buttonStyle =
    [ Style.basicButton Style.Big
    , width (px (Style.noteWidth Style.Big))
    , active [ Style.indent ]
    ]
        |> Css.batch
