module Data.Tracker.Collapse exposing
    ( Collapse(..)
    , all
    , areSame
    , decoder
    , encode
    , every
    , everyMajorMark
    , everyMinorMark
    , getEveryAmount
    , none
    , shouldShow
    , toLabel
    )

import Data.Beat exposing (Beat)
import Data.Encoding as Encoding
import Data.Index as Index exposing (Index)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- TYPES --


type Collapse
    = None
    | Every Int
    | EveryMajorMark
    | EveryMinorMark



-- VALUES --


all : Int -> List Collapse
all everyAmount =
    [ none
    , every everyAmount
    , everyMajorMark
    , everyMinorMark
    ]


none : Collapse
none =
    None


every : Int -> Collapse
every =
    Every


everyMajorMark : Collapse
everyMajorMark =
    EveryMajorMark


everyMinorMark : Collapse
everyMinorMark =
    EveryMinorMark



-- HELPERS --


shouldShow :
    { majorMark : Int
    , minorMark : Int
    , collapse : Collapse
    , beatIndex : Index (Beat Encoding.None)
    }
    -> Bool
shouldShow { majorMark, minorMark, collapse, beatIndex } =
    let
        remainderIsZero : Int -> Bool
        remainderIsZero basis =
            remainderBy basis (Index.toInt beatIndex) == 0
    in
    case collapse of
        None ->
            True

        Every everyAmount ->
            remainderIsZero everyAmount

        EveryMajorMark ->
            remainderIsZero majorMark

        EveryMinorMark ->
            remainderIsZero minorMark


toLabel : Collapse -> String
toLabel collapse =
    case collapse of
        None ->
            "none"

        Every int ->
            "every"

        EveryMajorMark ->
            "every major mark"

        EveryMinorMark ->
            "every minor mark"


getEveryAmount : Collapse -> Maybe Int
getEveryAmount collapse =
    case collapse of
        Every amount ->
            Just amount

        _ ->
            Nothing


areSame : Collapse -> Collapse -> Bool
areSame collapse0 collapse1 =
    case ( collapse0, collapse1 ) of
        ( None, None ) ->
            True

        ( Every _, Every _ ) ->
            True

        ( EveryMajorMark, EveryMajorMark ) ->
            True

        ( EveryMinorMark, EveryMinorMark ) ->
            True

        _ ->
            False


encode : Collapse -> Encode.Value
encode collapse =
    case collapse of
        None ->
            Encode.null

        Every everyAmount ->
            Encode.int everyAmount

        EveryMajorMark ->
            Encode.string "majorMark"

        EveryMinorMark ->
            Encode.string "minorMark"


decoder : Decoder Collapse
decoder =
    let
        fromString : String -> Decoder Collapse
        fromString str =
            case str of
                "majorMark" ->
                    Decode.succeed EveryMajorMark

                "minorMark" ->
                    Decode.succeed EveryMinorMark

                _ ->
                    Decode.fail "collapse isnt major or minor mark"
    in
    [ Decode.string
        |> Decode.andThen fromString
    , Decode.int
        |> Decode.map Every
    , Decode.null None
    ]
        |> Decode.oneOf
