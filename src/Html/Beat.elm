module Html.Beat
    exposing
        ( Msg(..)
        , update
        , view
        )

import Array exposing (Array)
import Colors
import Css exposing (..)
import Data.Part as Part
import Data.Tracker as Tracker
import Html.Buttons as Buttons
import Html.Grid as Grid
import Html.Note as Note
import Html.Styled as Html
    exposing
        ( Html
        , button
        )
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy
import Model exposing (Model)
import Return2 as R2
import Style


-- TYPES --


type Msg
    = NoteMsg Int Note.Msg
    | DeleteClicked
    | AddBelowClicked



-- UPDATE --


update : Int -> Int -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update ti si bi msg model =
    case msg of
        NoteMsg ni subMsg ->
            Note.update ti si bi ni subMsg model
                |> R2.mapCmd (NoteMsg ni)

        DeleteClicked ->
            Model.mapPart
                si
                (Part.removeBeat bi)
                model
                |> R2.withNoCmd

        AddBelowClicked ->
            Model.mapPart
                si
                (Part.addBeat bi)
                model
                |> R2.withNoCmd



-- VIEW --


view : Int -> Int -> Style.Size -> Int -> Int -> Array String -> Html Msg
view majorMark minorMark size ti bi beat =
    beat
        |> Array.toIndexedList
        |> List.map (wrapNote majorMark minorMark size ti bi)
        |> (::) (numberView size majorMark bi)
        |> (::) (Buttons.plus AddBelowClicked [] size)
        |> (::) (Buttons.delete DeleteClicked size)
        |> Grid.row []


wrapNote : Int -> Int -> Style.Size -> Int -> Int -> ( Int, String ) -> Html Msg
wrapNote majorMark minorMark size ti bi ( ni, str ) =
    Html.Styled.Lazy.lazy7
        Note.view
        majorMark
        minorMark
        size
        ti
        bi
        ni
        str
        |> Html.map (NoteMsg ni)


numberView : Style.Size -> Int -> Int -> Html Msg
numberView size majorMark index =
    button
        [ css [ numberStyle size ] ]
        [ Html.text (numberStr majorMark index) ]


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
    , margin (px 1)
    , width (px (Style.noteWidth size))
    , height (px (Style.noteHeight size))
    , backgroundColor Colors.ignorable2
    , color Colors.point0
    , Style.fontSmoothingNone
    , padding (px 0)
    , outline none
    ]
        |> Css.batch
