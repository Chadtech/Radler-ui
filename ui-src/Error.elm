module Error exposing
    ( initializationErrorView
    , runtimeErrorView
    )

import Css exposing (..)
import Data.Error as Error exposing (Error(..))
import Html.Grid as Grid
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Json.Decode as D
import Msg exposing (Msg(..))
import Style



-- VIEW --


{-| The view for when the app fails to initialize
-}
initializationErrorView : D.Error -> Html Msg
initializationErrorView error =
    Html.p
        [ Attrs.css [ errorPStyle ] ]
        [ Html.text (D.errorToString error) ]
        |> errorContainer


{-| Not a run time error in the sense that
the code crashed during run time, rather in the
sense there was a problem during run time
-}
runtimeErrorView : Error -> Html Msg
runtimeErrorView error =
    Html.p
        [ Attrs.css [ errorPStyle ] ]
        [ Html.text (Error.toString error) ]
        |> errorContainer


errorPStyle : Style
errorPStyle =
    [ Style.hfnss
    , property "word-wrap" "break-word"
    , maxWidth (px 500)
    ]
        |> Css.batch


errorContainer : Html Msg -> Html Msg
errorContainer child =
    Grid.container
        [ marginTop (pct 50)
        , transform (translateY (pct -50))
        ]
        [ Grid.row
            []
            [ Grid.column
                [ Style.card
                , Style.bigSpacing
                ]
                [ child ]
            ]
        ]
