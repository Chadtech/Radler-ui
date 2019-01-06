module Audio.Mono.Position
    ( positionMono
    ) where


import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Audio.Stereo (Stereo)
import qualified Audio.Stereo as Stereo
import qualified Constants
import Data.Function ((&))
import Position (Position)
import qualified Position
import Room (Room)
import qualified Room 
import Size (Size)
import qualified Size


import Prelude.Extra (debugLog)

positionMono :: Room -> Position -> Mono -> Stereo
positionMono room soundPosition mono =
    let
        listenerPosition :: Position
        listenerPosition =
            Room.listenerPosition room

        roomSize :: Size
        roomSize =
            Room.size room
    in
    Stereo.fromMonos
        ( positionRelativeToEar
            roomSize
            (leftEarPosition listenerPosition)
            soundPosition
            mono
        , positionRelativeToEar
            roomSize
            (rightEarPosition listenerPosition)
            soundPosition
            mono
        )


leftEarPosition :: Position -> Position
leftEarPosition =
    Position.addToX (-0.5)


rightEarPosition :: Position -> Position
rightEarPosition =
    Position.addToX 0.5


positionRelativeToEar :: Size -> Position -> Position -> Mono -> Mono
positionRelativeToEar roomSize earPosition soundPosition mono =
    [ directDelay earPosition soundPosition mono
    , delayFromFarWallReflection roomSize earPosition soundPosition mono
    , delayFromBackWallReflection earPosition soundPosition mono
    , delayFromLeftWallReflection earPosition soundPosition mono
    , delayFromFloorReflection earPosition soundPosition mono
    ]
        & Mono.mixMany

delayFromBackWallReflection :: Position -> Position -> Mono -> Mono
delayFromBackWallReflection earPosition soundPosition =
    let
        positionOnBackWall :: Position
        positionOnBackWall =
            reflectionPointOnBackWall
                earPosition
                soundPosition
    in
    delayAndDecay 
        ( Position.distanceBetween earPosition positionOnBackWall
            + Position.distanceBetween soundPosition positionOnBackWall
        )
        
reflectionPointOnBackWall :: Position -> Position -> Position
reflectionPointOnBackWall earPosition soundPosition =
    let
        relativeYDistanceFromBackWall :: Float
        relativeYDistanceFromBackWall =
            Position.x earPosition / (Position.x earPosition + Position.x soundPosition)
    in
    ( Position.x earPosition 
        + relativeYDistanceFromBackWall
        * (Position.x soundPosition - Position.x earPosition)
    , 0
    , Position.x earPosition 
        + relativeYDistanceFromBackWall
        * (Position.x soundPosition - Position.x earPosition)
    )
        & Position.fromCoords


delayFromFloorReflection :: Position -> Position -> Mono -> Mono
delayFromFloorReflection earPosition soundPosition =
    let
        positionOnFloor :: Position
        positionOnFloor =
            reflectionPointOnFloor
                earPosition
                soundPosition
    in
    delayAndDecay 
        ( Position.distanceBetween earPosition positionOnFloor
            + Position.distanceBetween soundPosition positionOnFloor
        )


reflectionPointOnFloor :: Position -> Position -> Position
reflectionPointOnFloor earPosition soundPosition =
    let
        relativeZDistanceFromFloor :: Float
        relativeZDistanceFromFloor =
            Position.x earPosition / (Position.x earPosition + Position.x soundPosition)
    in
    ( Position.x earPosition 
        + relativeZDistanceFromFloor
        * (Position.x soundPosition - Position.x earPosition)
    , Position.y earPosition 
        + relativeZDistanceFromFloor
        * (Position.y soundPosition - Position.y earPosition)
    , 0
    )
        & Position.fromCoords


delayFromLeftWallReflection :: Position -> Position -> Mono -> Mono
delayFromLeftWallReflection earPosition soundPosition =
    let
        positionOnLeftWall :: Position
        positionOnLeftWall =
            reflectionPointOnLeftWall
                earPosition
                soundPosition
    in
    delayAndDecay 
        ( Position.distanceBetween earPosition positionOnLeftWall
            + Position.distanceBetween soundPosition positionOnLeftWall
        )


reflectionPointOnLeftWall :: Position -> Position -> Position
reflectionPointOnLeftWall earPosition soundPosition =
    let
        relativeXDistanceFromLeftWall :: Float
        relativeXDistanceFromLeftWall =
            Position.x earPosition / (Position.x earPosition + Position.x soundPosition)
    in
    ( 0
    , Position.y earPosition 
        + relativeXDistanceFromLeftWall
        * (Position.y soundPosition - Position.y earPosition)
    , Position.z earPosition 
        + relativeXDistanceFromLeftWall
        * (Position.z soundPosition - Position.z earPosition)
    )
        & Position.fromCoords


delayFromFarWallReflection :: Size -> Position -> Position -> Mono -> Mono
delayFromFarWallReflection roomSize earPosition soundPosition =
    let
        positionOnFarWall :: Position
        positionOnFarWall =
            reflectionPointOnFarWall
                roomSize
                earPosition
                soundPosition
    in
    delayAndDecay 
        ( Position.distanceBetween earPosition positionOnFarWall
            + Position.distanceBetween soundPosition positionOnFarWall
        )


reflectionPointOnFarWall :: Size -> Position -> Position -> Position
reflectionPointOnFarWall roomSize earPosition soundPosition =
    let
        earYPositionFromFarWall :: Float
        earYPositionFromFarWall =
            Size.length roomSize - Position.y earPosition

        relativeYDistanceFromFarWall :: Float
        relativeYDistanceFromFarWall =
            earYPositionFromFarWall
                / (earYPositionFromFarWall + (Size.length roomSize - Position.y soundPosition))
    in
    ( Position.x earPosition 
        + relativeYDistanceFromFarWall 
        * (Position.x soundPosition - Position.x earPosition)
    , Size.length roomSize
    , Position.z earPosition 
        + relativeYDistanceFromFarWall 
        * (Position.z soundPosition - Position.z earPosition)
    )
        & Position.fromCoords


directDelay :: Position -> Position -> Mono -> Mono
directDelay earPosition soundPosition =
    delayAndDecay $ Position.distanceBetween earPosition soundPosition


delayAndDecay :: Float -> Mono -> Mono
delayAndDecay distance =
    Mono.setVolume (1 / (distance ^ 2)) . Mono.delay (calcDelay distance)


calcDelay :: Float -> Int
calcDelay distance =
    round (distance / Constants.speedOfSound)