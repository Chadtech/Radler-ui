module Note
    exposing
        ( Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Browser.Dom as Dom
import Colors
import Css exposing (..)
import Data.Beat as Beat
import Data.Note as Note exposing (Note)
import Data.Part as Part
import Data.Tracker as Tracker
import Html.Custom
    exposing
        ( Arrow(..)
        , onArrowKey
        , onEnter
        )
import Html.Grid as Grid
import Html.Styled as Html exposing (Html, input)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onInput)
import Model exposing (Model)
import Return2 as R2
import Style
import Task


-- TYPES --


type Msg
    = Updated Note
    | ArrowPressed Arrow
    | EnterPressed
    | NoteFocused



-- UPDATE --


update : Int -> Int -> Int -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update ti si bi ni msg model =
    case msg of
        Updated note ->
            Beat.setNote ni note
                |> Part.mapBeat bi
                |> Model.mapPart si
                |> Model.apply model
                |> R2.withNoCmd

        ArrowPressed Up ->
            noteId ti (bi - 1) ni
                |> focusOnNote
                |> R2.withModel model

        ArrowPressed Down ->
            noteId ti (bi + 1) ni
                |> focusOnNote
                |> R2.withModel model

        ArrowPressed Left ->
            noteId ti bi (ni - 1)
                |> focusOnNote
                |> R2.withModel model

        ArrowPressed Right ->
            noteId ti bi (ni + 1)
                |> focusOnNote
                |> R2.withModel model

        NoteFocused ->
            model
                |> R2.withNoCmd

        EnterPressed ->
            noteId ti (bi + 1) ni
                |> focusOnNote
                |> R2.withModel model


focusOnNote : String -> Cmd Msg
focusOnNote id =
    Task.attempt (always NoteFocused) (Dom.focus id)



-- VIEW --


view : Int -> Int -> Style.Size -> Int -> Int -> Int -> Note -> Html Msg
view majorMark minorMark size ti bi ni note =
    Grid.column
        [ margin (px 1)
        , marginBottom (px 0)
        ]
        [ input
            [ Attrs.css
                [ style
                    majorMark
                    minorMark
                    size
                    bi
                ]
            , Attrs.value (Note.toString note)
            , Attrs.spellcheck False
            , onInput (Updated << Note.fromString)
            , onArrowKey ArrowPressed
            , onEnter EnterPressed
            , Attrs.id (noteId ti bi ni)
            ]
            []
        ]


style : Int -> Int -> Style.Size -> Int -> Style
style majorMark minorMark size beatIndex =
    [ Style.basicInput
    , determineNoteBgColor
        majorMark
        minorMark
        beatIndex
        |> backgroundColor
    , Style.font size
    , color Colors.point0
    , width (px (Style.noteWidth size))
    , Style.fontSmoothingNone
    ]
        |> Css.batch


determineNoteBgColor : Int -> Int -> Int -> Color
determineNoteBgColor majorMark minorMark beatIndex =
    case remainderBy majorMark beatIndex of
        0 ->
            Colors.highlight1

        moduloMajorMark ->
            if remainderBy minorMark moduloMajorMark == 0 then
                Colors.highlight0
            else
                Colors.background2


noteId : Int -> Int -> Int -> String
noteId ti bi ni =
    [ "t"
    , String.fromInt ti
    , "b"
    , String.fromInt bi
    , "n"
    , String.fromInt ni
    ]
        |> String.concat
