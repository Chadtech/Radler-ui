module Note exposing
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
import Html.Grid as Grid
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as D exposing (Decoder)
import Model exposing (Model)
import Return2 as R2
import Style
import Task



-- TYPES --


type Msg
    = Updated Note
    | MovementKeyPressed Direction
    | NoteFocused



-- UPDATE --


update : Int -> Int -> Int -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update ti si bi ni msg model =
    case msg of
        Updated note ->
            model
                |> updateBeat si bi ni note
                |> R2.withNoCmd

        MovementKeyPressed Up ->
            noteId ti (bi - 1) ni
                |> focusOnNote
                |> R2.withModel model

        MovementKeyPressed Down ->
            noteId ti (bi + 1) ni
                |> focusOnNote
                |> R2.withModel model

        MovementKeyPressed Left ->
            noteId ti bi (ni - 1)
                |> focusOnNote
                |> R2.withModel model

        MovementKeyPressed Right ->
            noteId ti bi (ni + 1)
                |> focusOnNote
                |> R2.withModel model

        NoteFocused ->
            model
                |> R2.withNoCmd


updateBeat : Int -> Int -> Int -> Note -> Model -> Model
updateBeat si bi ni note =
    note
        |> Beat.setNote ni
        |> Part.mapBeat bi
        |> Model.mapPart si


focusOnNote : String -> Cmd Msg
focusOnNote id =
    Task.attempt
        (always NoteFocused)
        (Dom.focus id)



-- VIEW --


view : Int -> Int -> Style.Size -> Int -> Int -> Int -> Note -> Html Msg
view majorMark minorMark size ti bi ni note =
    Grid.column
        [ margin (px 1) ]
        [ Html.input
            [ Attrs.css
                [ style
                    majorMark
                    minorMark
                    size
                    bi
                ]
            , Attrs.value (Note.toString note)
            , Attrs.spellcheck False
            , Events.onInput (Updated << Note.fromString)
            , onMovementKey MovementKeyPressed
            , Attrs.id (noteId ti bi ni)
            ]
            []
        ]


style : Int -> Int -> Style.Size -> Int -> Style
style majorMark minorMark size beatIndex =
    [ determineNoteBgColor
        majorMark
        minorMark
        beatIndex
        |> backgroundColor
    , Style.font size
    , color Colors.point0
    , height (px (Style.noteHeight size))
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


type Direction
    = Up
    | Down
    | Right
    | Left


onMovementKey : (Direction -> msg) -> Attribute msg
onMovementKey ctor =
    Events.keyCode
        |> D.andThen directionDecoder
        |> D.map ctor
        |> Events.on "keydown"


directionDecoder : Int -> Decoder Direction
directionDecoder key =
    case key of
        -- Arrow keys --
        37 ->
            D.succeed Left

        39 ->
            D.succeed Right

        38 ->
            D.succeed Up

        40 ->
            D.succeed Down

        -- Enter Key
        13 ->
            D.succeed Down

        _ ->
            D.fail "Key isnt a direction key (arrows and enter)"
