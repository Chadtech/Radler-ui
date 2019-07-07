module Data.Room exposing
    ( Room
    , decoder
    , toString
    )

import Json.Decode as Decode exposing (Decoder)



-- TYPES --


type alias Room =
    { listenerPosition : Position
    , size : Size
    }


type alias Position =
    { x : Int
    , y : Int
    , z : Int
    }


type alias Size =
    { width : Int
    , length : Int
    , height : Int
    }



-- HELPERS --


toString : Room -> String
toString room =
    [ "x="
    , String.fromInt room.listenerPosition.x
    , "y="
    , String.fromInt room.listenerPosition.y
    , "z="
    , String.fromInt room.listenerPosition.z
    , "width="
    , String.fromInt room.size.width
    , "length="
    , String.fromInt room.size.length
    , "height="
    , String.fromInt room.size.height
    ]
        |> String.concat



-- DECODER --


decoder : Decoder Room
decoder =
    Decode.map2 Room
        (Decode.field "listener-position" positionDecoder)
        (Decode.field "size" sizeDecoder)


positionDecoder : Decoder Position
positionDecoder =
    Decode.map3 Position
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "z" Decode.int)


sizeDecoder : Decoder Size
sizeDecoder =
    Decode.map3 Size
        (Decode.field "width" Decode.int)
        (Decode.field "length" Decode.int)
        (Decode.field "height" Decode.int)
