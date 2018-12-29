module Data.Room exposing
    ( Room
    , decoder
    , toString
    )

import Json.Decode as JD exposing (Decoder)



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
    JD.map2 Room
        (JD.field "listener-position" positionDecoder)
        (JD.field "size" sizeDecoder)


positionDecoder : Decoder Position
positionDecoder =
    JD.map3 Position
        (JD.field "x" JD.int)
        (JD.field "y" JD.int)
        (JD.field "z" JD.int)


sizeDecoder : Decoder Size
sizeDecoder =
    JD.map3 Size
        (JD.field "width" JD.int)
        (JD.field "length" JD.int)
        (JD.field "height" JD.int)
