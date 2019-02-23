module Ui.Beat exposing
    ( Msg(..)
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Beat as Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Note as Note exposing (Note)
import Data.Part as Part
import Html.Buttons as Buttons
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Html
        , button
        )
import Html.Styled.Attributes as Attrs
import Html.Styled.Lazy
import Model exposing (Model)
import Style
import Ui.Note as Note
import Util



-- TYPES --


type Msg
    = NoteMsg Int Note.Msg
    | DeleteClicked
    | AddBelowClicked



-- UPDATE --


{-|

    Theres a lot of indexing going on!

        ti := tracker index
        pi := part index
        bi := beat index
        ni := note index

-}
update : Int -> Int -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update ti pi bi msg model =
    case msg of
        NoteMsg ni subMsg ->
            Note.update ti pi bi ni subMsg model
                |> Util.mapCmd (NoteMsg ni)

        DeleteClicked ->
            Model.mapPart
                pi
                (Part.removeBeat bi)
                model
                |> Util.withNoCmd

        AddBelowClicked ->
            Model.mapPart
                pi
                (Part.addBeatBelow bi)
                model
                |> Util.withNoCmd



-- VIEW --


view : Int -> Int -> Style.Size -> Int -> Int -> Beat Encoding.None -> Html Msg
view majorMark minorMark size ti bi beat =
    beat
        |> Beat.toIndexedList
        |> List.map (wrapNote majorMark minorMark size ti bi)
        |> (::) (numberView size majorMark bi)
        |> (::) (plusButton size)
        |> (::) (deleteButton size)
        |> Grid.row []


plusButton : Style.Size -> Html Msg
plusButton size =
    Grid.column
        [ margin (px 1) ]
        [ Buttons.plus
            AddBelowClicked
            [ width (px (Style.noteWidth size / 2)) ]
            size
        ]


deleteButton : Style.Size -> Html Msg
deleteButton size =
    Grid.column
        [ margin (px 1) ]
        [ Buttons.delete
            DeleteClicked
            [ width (px (Style.noteWidth size / 2)) ]
            size
        ]


wrapNote : Int -> Int -> Style.Size -> Int -> Int -> ( Int, Note Encoding.None ) -> Html Msg
wrapNote majorMark minorMark size ti bi ( ni, note ) =
    Html.Styled.Lazy.lazy7
        Note.view
        majorMark
        minorMark
        size
        ti
        bi
        ni
        note
        |> Html.map (NoteMsg ni)


numberView : Style.Size -> Int -> Int -> Html Msg
numberView size majorMark index =
    Grid.column
        [ margin (px 1) ]
        [ button
            [ Attrs.css [ numberStyle size ] ]
            [ Html.text (numberStr majorMark index) ]
        ]


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


numberStyle : Style.Size -> Style
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
