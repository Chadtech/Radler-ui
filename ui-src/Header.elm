module Header exposing
    ( Msg
    , update
    , view
    )

import Array
import Colors
import Css exposing (..)
import Data.Error as Error
import Data.Package as Package
import Data.Part as Part
import Data.Tracker as Tracker
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Http
import Json.Decode as D
import Model exposing (Model, Page)
import Return2 as R2
import Style



-- TYPES --


type Msg
    = PageClicked Page
    | NewSheetClicked
    | NewTrackerClicked
    | PlayClicked
    | OpenClicked
    | SaveClicked
    | PlayFromFieldUpdated String
    | PlayForFieldUpdated String
    | PlaySent (Result Http.Error ())



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageClicked page ->
            { model | page = page }
                |> R2.withNoCmd

        NewSheetClicked ->
            { model
                | parts =
                    Array.push Part.empty model.parts
            }
                |> R2.withNoCmd

        NewTrackerClicked ->
            { model
                | trackers =
                    Array.push
                        (Tracker.init Style.Small 0)
                        model.trackers
            }
                |> R2.withNoCmd

        PlayClicked ->
            case Model.score model of
                Ok scoreStr ->
                    scoreStr
                        |> Play
                        |> sendHttp model
                        |> R2.withModel model

                Err newModel ->
                    newModel
                        |> R2.withNoCmd

        OpenClicked ->
            model
                |> R2.withNoCmd

        SaveClicked ->
            [ Package.saveToDisk
                model.package
            , Model.saveParts
                model
            ]
                |> Cmd.batch
                |> R2.withModel model

        PlayFromFieldUpdated str ->
            model
                |> Model.setPlayFrom str
                |> R2.withNoCmd

        PlayForFieldUpdated str ->
            model
                |> Model.setPlayFor str
                |> R2.withNoCmd

        PlaySent (Ok ()) ->
            model
                |> R2.withNoCmd

        PlaySent (Err err) ->
            playFailed (Debug.log "ERROR" err) model
                |> R2.withNoCmd


playFailed : Http.Error -> Model -> Model
playFailed error =
    error
        |> playErrorToString
        |> Error.BackendHadProblemWithScore
        |> Model.setError


playErrorToString : Http.Error -> String
playErrorToString error =
    case error of
        Http.BadUrl url ->
            "bad url -> " ++ url

        Http.Timeout ->
            "time out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus response ->
            [ String.fromInt response.status.code
            , response.status.message
            , response.body
            ]
                |> String.join " - "

        Http.BadPayload decodeError _ ->
            "Decoder problem -> " ++ decodeError



-- HTTP --


type Call
    = Play String


sendHttp : Model -> Call -> Cmd Msg
sendHttp model call =
    case call of
        Play score ->
            Http.post
                (Model.urlRoute model "play")
                (Http.stringBody "string" score)
                (D.null ())
                |> Http.send PlaySent



-- VIEW --


view : Model -> Html Msg
view model =
    Grid.row
        [ Style.card
        , displayFlex
        , minHeight minContent
        ]
        [ Grid.column
            [ flex (int 0) ]
            [ saveButton ]
        , Grid.column
            [ flex (int 0) ]
            [ playButton ]
        , Grid.column
            [ flex (int 0)
            , margin2 (px 0) (px 10)
            ]
            [ fromText ]
        , Grid.column
            [ flex (int 0) ]
            [ playFromField model.playFromBeatField ]
        , Grid.column
            [ flex (int 0)
            , margin2 (px 0) (px 10)
            ]
            [ forText ]
        , Grid.column
            [ flex (int 0) ]
            [ playForField model.playForBeatsField ]
        , Grid.column
            [ flex (int 0)
            , marginLeft (px 15)
            ]
            [ trackersButton model.page ]
        , Grid.column
            [ flex (int 0) ]
            [ packageButton model.page ]
        , Grid.column
            [ marginLeft (px 10)
            , flex (int 0)
            ]
            [ newSheetButton ]
        , Grid.column
            [ flex (int 0) ]
            [ newTrackerButton ]
        ]


fromText : Html Msg
fromText =
    Html.p
        [ Attrs.css
            [ Style.hfnss
            , lineHeight (px 32)
            ]
        ]
        [ Html.text "from" ]


forText : Html Msg
forText =
    Html.p
        [ Attrs.css
            [ Style.hfnss
            , lineHeight (px 32)
            ]
        ]
        [ Html.text "for" ]


playForField : String -> Html Msg
playForField playForBeats =
    Html.input
        [ Attrs.css
            [ margin2 (px 1) (px 0)
            , Style.hfnss
            , color Colors.point0
            , Style.fontSmoothingNone
            , width (px ((Style.noteWidth Style.Big * 1.5) / 2))
            ]
        , Attrs.value playForBeats
        , Attrs.spellcheck False
        , Events.onInput PlayForFieldUpdated
        ]
        []


playFromField : String -> Html Msg
playFromField playFromBeat =
    Html.input
        [ Attrs.css
            [ margin2 (px 1) (px 0)
            , Style.hfnss
            , color Colors.point0
            , Style.fontSmoothingNone
            , width (px ((Style.noteWidth Style.Big * 1.5) / 2))
            ]
        , Attrs.value playFromBeat
        , Attrs.spellcheck False
        , Events.onInput PlayFromFieldUpdated
        ]
        []


playButton : Html Msg
playButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick PlayClicked
        ]
        [ Html.text "play" ]


openButton : Html Msg
openButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick OpenClicked
        ]
        [ Html.text "open" ]


saveButton : Html Msg
saveButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick SaveClicked
        ]
        [ Html.text "save" ]


trackersButton : Page -> Html Msg
trackersButton page =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , dent page Model.Trackers
            ]
        , Events.onClick (PageClicked Model.Trackers)
        ]
        [ Html.text "trackers" ]


packageButton : Page -> Html Msg
packageButton page =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , dent page Model.Package
            ]
        , Events.onClick (PageClicked Model.Package)
        ]
        [ Html.text "package" ]


newSheetButton : Html Msg
newSheetButton =
    Html.button
        [ Attrs.css [ buttonStyle ]
        , Events.onClick NewSheetClicked
        ]
        [ Html.text "new part" ]


newTrackerButton : Html Msg
newTrackerButton =
    Html.button
        [ Attrs.css
            [ buttonStyle
            , width (px (Style.noteWidth Style.Big * 2))
            ]
        , Events.onClick NewTrackerClicked
        ]
        [ Html.text "new tracker" ]


buttonStyle : Style
buttonStyle =
    [ Style.hfnss
    , margin (px 1)
    , width (px (Style.noteWidth Style.Big * 1.5))
    , height (px (Style.noteHeight Style.Big + 4))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , outline none
    , active [ Style.indent ]
    , Style.outdent
    ]
        |> Css.batch


dent : Page -> Page -> Style
dent currentPage thisPage =
    if currentPage == thisPage then
        Style.indent

    else
        Style.outdent
