module Details
    exposing
        ( Model
        , Msg
        , update
        , view
        )

--import Row

import Css exposing (..)
import Html.Custom exposing (p)
import Html.Grid as Grid
import Html.Styled as Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , input
        )
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        )
import Style


-- TYPES --


type alias Model =
    { sheetNameField : String }


type Msg
    = None



-- UDPATE --


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model



-- VIEW --


view : Model -> Html Msg
view model =
    div
        [ css
            [ Style.card
            , transform (translate2 (pct -50) (pct -50))
            , position absolute
            , top (pct 50)
            , left (pct 50)
            ]
        ]
        [ Grid.container
            [ css [ padding (px 1) ] ]
            [ Grid.row [] (sheetNameInput model)
            , Grid.row
                [ css [ justifyContent spaceAround ] ]
                [ sheetSaveButton ]
            ]
        ]


sheetNameInput : Model -> List (Html Msg)
sheetNameInput model =
    [ Grid.column
        [ css
            [ lineHeight (px 26) ]
        ]
        [ p
            [ css
                [ Style.hfnss
                , marginRight (px 10)
                , marginLeft (px 10)
                ]
            ]
            [ Html.text "sheet name" ]
        ]
    , Grid.column
        []
        [ input
            [ css
                [ Style.basicInput
                , Style.hfnss
                ]
            ]
            []
        ]
    ]


sheetSaveButton : Html Msg
sheetSaveButton =
    Grid.column
        [ css [ flex none ] ]
        [ button
            [ css
                [ Style.basicButton Style.Big
                , width (px (Style.cellWidth Style.Big * 2))
                ]
            ]
            [ Html.text "save" ]
        ]
