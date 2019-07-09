module Ui.Beat exposing
    ( Msg(..)
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Index as Index exposing (Index)
import Data.Note as Note exposing (Note)
import Data.Part as Part exposing (Part)
import Data.Size exposing (Size)
import Data.Tracker exposing (Tracker)
import Data.Tracker.Collapse as Collapse exposing (Collapse)
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Lazy
import Model exposing (Model)
import Style
import Ui.Note as Note
import Util.Cmd as CmdUtil
import View.Button as Button



-- TYPES --


type Msg
    = NoteMsg (Index (Note Encoding.None)) Note.Msg
    | DeleteClicked
    | AddBelowClicked



-- UPDATE --


update :
    Index Tracker
    -> Index Part
    -> Index (Beat Encoding.None)
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update trackerIndex partIndex beatIndex msg model =
    case msg of
        NoteMsg noteIndex subMsg ->
            Note.update
                trackerIndex
                partIndex
                beatIndex
                noteIndex
                subMsg
                model
                |> CmdUtil.mapCmd (NoteMsg noteIndex)

        DeleteClicked ->
            Model.mapPart
                partIndex
                (Part.removeBeat beatIndex)
                model
                |> CmdUtil.withNoCmd

        AddBelowClicked ->
            Model.mapPart
                partIndex
                (Part.addBeatBelow beatIndex)
                model
                |> CmdUtil.withNoCmd



-- VIEW --


view :
    Int
    -> Int
    -> Size
    -> Collapse
    -> Index Tracker
    -> Index (Beat Encoding.None)
    -> Beat Encoding.None
    -> Html Msg
view majorMark minorMark size collapse trackerIndex beatIndex beat =
    if
        Collapse.shouldShow
            { majorMark = majorMark
            , minorMark = minorMark
            , collapse = collapse
            , beatIndex = beatIndex
            }
    then
        beat
            |> Beat.toIndexedList
            |> List.map (wrapNote majorMark minorMark size trackerIndex beatIndex)
            |> (::) (numberView size majorMark beatIndex)
            |> (::) (buttonColumn AddBelowClicked "+v" size)
            |> (::) (buttonColumn DeleteClicked "x" size)
            |> Grid.row []

    else
        Html.text ""


buttonColumn : Msg -> String -> Size -> Html Msg
buttonColumn msg label size =
    Grid.column
        [ margin (px 1) ]
        [ Button.button msg label
            |> Button.withWidth Button.halfWidth
            |> Button.withSize size
            |> Button.toHtml
        ]


wrapNote :
    Int
    -> Int
    -> Size
    -> Index Tracker
    -> Index (Beat Encoding.None)
    -> ( Index (Note Encoding.None), Note Encoding.None )
    -> Html Msg
wrapNote majorMark minorMark size trackerIndex beatIndex ( noteIndex, note ) =
    Html.Styled.Lazy.lazy7
        Note.view
        majorMark
        minorMark
        size
        trackerIndex
        beatIndex
        noteIndex
        note
        |> Html.map (NoteMsg noteIndex)


numberView : Size -> Int -> Index (Beat Encoding.None) -> Html Msg
numberView size majorMark index =
    Grid.column
        [ margin (px 1) ]
        [ Html.button
            [ Attrs.css [ numberStyle size ] ]
            [ Html.text (numberStr majorMark <| Index.toInt index) ]
        ]


numberStyle : Size -> Style
numberStyle size =
    [ Style.outdent
    , Style.font size
    , width (px (Style.noteWidth size))
    , height (px (Style.noteHeight size))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    , Style.flush
    ]
        |> Css.batch


numberStr : Int -> Int -> String
numberStr majorMark index =
    [ String.fromInt (index // majorMark)
    , "."
    , beatNumber
        (remainderBy majorMark index)
        "0123456789abcdefghijklmnopqrstuv"
    ]
        |> String.concat


beatNumber : Int -> String -> String
beatNumber i str =
    if i == 0 then
        String.left 1 str

    else
        beatNumber (i - 1) (String.dropLeft 1 str)
