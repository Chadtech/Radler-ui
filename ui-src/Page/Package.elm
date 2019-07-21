module Page.Package exposing
    ( Msg
    , update
    , view
    )

import Colors
import Css exposing (..)
import Data.Package as Package
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Model exposing (Model)
import Util.Css as CssUtil
import View.TextArea as TextArea



-- TYPES --


type Msg
    = PackageUpdated String



-- UPDATE --


update : Msg -> Model -> Model
update msg =
    case msg of
        PackageUpdated str ->
            str
                |> Package.setJsonStrField
                |> Model.mapPackage



-- VIEW --


view : Model -> List (Grid.Column Msg)
view model =
    [ Grid.column
        [ margin (px 1) ]
        [ TextArea.config PackageUpdated model.package.jsonStrField
            |> TextArea.withStyles [ maybeInvalidStyles model ]
            |> TextArea.toHtml
        ]
    ]


maybeInvalidStyles : Model -> Style
maybeInvalidStyles { package } =
    [ backgroundColor Colors.lowWarning
    , border3 (px 2) solid Colors.critical
    ]
        |> Css.batch
        |> CssUtil.styleIf (not package.validJson)
