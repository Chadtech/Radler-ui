module Ui.Note exposing
    ( Msg(..)
    , update
    , view
    )

import Browser.Dom as Dom
import Colors
import Css exposing (..)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Index as Index exposing (Index)
import Data.Note as Note exposing (Note)
import Data.Part as Part exposing (Part)
import Data.Size as Size exposing (Size)
import Data.Tracker exposing (Tracker)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Model exposing (Model)
import Task
import Util.Cmd as CmdUtil
import View.Input as Input



-- TYPES --


type Msg
    = Updated String
    | MovementKeyPressed Direction
    | NoteFocused (Result Dom.Error ())



-- UPDATE --


update :
    Index Tracker
    -> Index Part
    -> Index (Beat Encoding.None)
    -> Index (Note Encoding.None)
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update trackerIndex partIndex beatIndex noteIndex msg model =
    case msg of
        Updated noteStr ->
            model
                |> updateBeat
                    partIndex
                    beatIndex
                    noteIndex
                    (Note.fromString noteStr)
                |> CmdUtil.withNoCmd

        MovementKeyPressed Up ->
            noteId trackerIndex (Index.previous beatIndex) noteIndex
                |> focusOnNote
                |> CmdUtil.withModel model

        MovementKeyPressed Down ->
            noteId trackerIndex (Index.next beatIndex) noteIndex
                |> focusOnNote
                |> CmdUtil.withModel model

        MovementKeyPressed Left ->
            noteId trackerIndex beatIndex (Index.previous noteIndex)
                |> focusOnNote
                |> CmdUtil.withModel model

        MovementKeyPressed Right ->
            noteId trackerIndex beatIndex (Index.next noteIndex)
                |> focusOnNote
                |> CmdUtil.withModel model

        NoteFocused _ ->
            model
                |> CmdUtil.withNoCmd


updateBeat : Index Part -> Index (Beat Encoding.None) -> Index (Note Encoding.None) -> Note Encoding.None -> Model -> Model
updateBeat partIndex beatIndex noteIndex note =
    note
        |> Beat.setNote noteIndex
        |> Part.mapBeat beatIndex
        |> Model.mapPart partIndex


focusOnNote : String -> Cmd Msg
focusOnNote id =
    Task.attempt
        NoteFocused
        (Dom.focus id)



-- VIEW --


view : Int -> Int -> Int -> Int -> Int -> Size -> Note Encoding.None -> Html Msg
view trackerIndexInt beatIndexInt noteIndexInt majorMark minorMark size note =
    let
        trackerIndex : Index Tracker
        trackerIndex =
            Index.fromInt trackerIndexInt

        beatIndex : Index (Beat Encoding.None)
        beatIndex =
            Index.fromInt beatIndexInt

        noteIndex : Index (Note Encoding.None)
        noteIndex =
            Index.fromInt noteIndexInt

        bgColor : Color
        bgColor =
            case
                remainderBy majorMark <|
                    Index.toInt beatIndex
            of
                0 ->
                    Colors.highlight1

                moduloMajorMark ->
                    if remainderBy minorMark moduloMajorMark == 0 then
                        Colors.highlight0

                    else
                        Colors.background2
    in
    Input.config Updated (Note.toString note)
        |> Input.withSize size
        |> Input.withBackgroundColor bgColor
        |> Input.withHtmlAttr (onMovementKey MovementKeyPressed)
        |> Input.withId (noteId trackerIndex beatIndex noteIndex)
        |> Input.toHtml


noteId : Index Tracker -> Index (Beat Encoding.None) -> Index (Note Encoding.None) -> String
noteId trackerIndex beatIndex noteIndex =
    [ "t"
    , Index.toString trackerIndex
    , "b"
    , Index.toString beatIndex
    , "n"
    , Index.toString noteIndex
    ]
        |> String.concat


type Direction
    = Up
    | Down
    | Right
    | Left


onMovementKey : (Direction -> msg) -> Attribute msg
onMovementKey ctor =
    Decode.map2 Tuple.pair
        (Decode.field "metaKey" Decode.bool)
        Events.keyCode
        |> Decode.andThen directionDecoder
        |> Decode.map ctor
        |> Events.on "keydown"


directionDecoder : ( Bool, Int ) -> Decoder Direction
directionDecoder ( metaKey, key ) =
    if metaKey then
        case key of
            -- Arrow keys --
            37 ->
                Decode.succeed Left

            39 ->
                Decode.succeed Right

            38 ->
                Decode.succeed Up

            40 ->
                Decode.succeed Down

            _ ->
                Decode.fail "Key isnt a direction key"

    else if key == 13 then
        Decode.succeed Down

    else
        Decode.fail "Key is not enter"
