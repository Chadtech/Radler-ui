module Mono.Position
    ( positionMono
    ) where

import Prelude.Extra
import Flow

import Mono (Mono)
import qualified Mono
import Stereo (Stereo)
import qualified Stereo

import qualified Constants
import Position (Position)
import qualified Position
import Room (Room)
import qualified Room 
import Size (Size)
import qualified Size
import Duration (Duration(Duration))
import Volume (Volume(..))



positionMono :: Room -> Position -> Mono -> Stereo
positionMono room soundPosition !mono =
    ( positionRelativeToEar
        (Room.mapListenerPosition leftEarPosition room)
        soundPosition
        mono
    , positionRelativeToEar
        (Room.mapListenerPosition rightEarPosition room)
        soundPosition
        mono
    )
        |> Stereo.fromMonos


leftEarPosition :: Position -> Position
leftEarPosition =
    Position.addToX (-0.5)


rightEarPosition :: Position -> Position
rightEarPosition =
    Position.addToX 0.5


positionRelativeToEar :: Room -> Position -> Mono -> Mono
positionRelativeToEar room soundPosition !mono =
    let
        roomSize :: Size
        roomSize =
            Room.size room

        earPosition :: Position
        earPosition =
            Room.listenerPosition room
    in
    [ directDelay earPosition soundPosition mono
    , delayFromFarWallReflection roomSize earPosition soundPosition mono
    , delayFromRightWallReflection roomSize earPosition soundPosition mono
    , delayFromCeilingReflection roomSize earPosition soundPosition mono
    , delayFromBackWallReflection earPosition soundPosition mono
    , delayFromLeftWallReflection earPosition soundPosition mono
    , delayFromFloorReflection earPosition soundPosition mono
    ]
        |> Mono.mixMany



delayFromBackWallReflection :: Position -> Position -> Mono -> Mono
delayFromBackWallReflection earPosition soundPosition =
    let
        positionOnBackWall :: Position
        positionOnBackWall =
            reflectionPointOnBackWall
                earPosition
                soundPosition
    in
    delayAndDecay <|
        Position.distanceBetween earPosition positionOnBackWall
            + Position.distanceBetween soundPosition positionOnBackWall
        

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
        |> Position.fromCoords


delayFromFloorReflection :: Position -> Position -> Mono -> Mono
delayFromFloorReflection earPosition soundPosition =
    let
        positionOnFloor :: Position
        positionOnFloor =
            reflectionPointOnFloor
                earPosition
                soundPosition
    in
    delayAndDecay <|
        Position.distanceBetween earPosition positionOnFloor
            + Position.distanceBetween soundPosition positionOnFloor


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
        |> Position.fromCoords


delayFromLeftWallReflection :: Position -> Position -> Mono -> Mono
delayFromLeftWallReflection earPosition soundPosition =
    let
        positionOnLeftWall :: Position
        positionOnLeftWall =
            reflectionPointOnLeftWall
                earPosition
                soundPosition
    in
    delayAndDecay <|
        Position.distanceBetween earPosition positionOnLeftWall
            + Position.distanceBetween soundPosition positionOnLeftWall


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
        |> Position.fromCoords


delayFromCeilingReflection :: Size -> Position -> Position -> Mono -> Mono
delayFromCeilingReflection roomSize earPosition soundPosition =
    let
        positionOnCeiling :: Position
        positionOnCeiling =
            reflectionPointOnCeiling
                roomSize
                earPosition
                soundPosition
    in
    delayAndDecay <|
        Position.distanceBetween earPosition positionOnCeiling
            + Position.distanceBetween soundPosition positionOnCeiling


reflectionPointOnCeiling :: Size -> Position -> Position -> Position
reflectionPointOnCeiling roomSize earPosition soundPosition =
    let
        earZPositionFromCeiling :: Float
        earZPositionFromCeiling =
            Size.height roomSize - Position.z earPosition

        relativeZDistanceFromCeiling :: Float
        relativeZDistanceFromCeiling =
            earZPositionFromCeiling
                / (earZPositionFromCeiling + (Size.height roomSize - Position.z soundPosition))
    in
    ( Position.x earPosition
        + relativeZDistanceFromCeiling
        * (Position.x soundPosition - Position.x earPosition)
    , Position.y earPosition
        + relativeZDistanceFromCeiling
        * (Position.z soundPosition - Position.z earPosition)
    , Size.height roomSize
    )
        |> Position.fromCoords


delayFromRightWallReflection :: Size -> Position -> Position -> Mono -> Mono
delayFromRightWallReflection roomSize earPosition soundPosition =
    let
        positionOnRightWall :: Position
        positionOnRightWall =
            reflectionPointOnRightWall
                roomSize
                earPosition
                soundPosition
    in
    delayAndDecay <|
        Position.distanceBetween earPosition positionOnRightWall
            + Position.distanceBetween soundPosition positionOnRightWall


reflectionPointOnRightWall :: Size -> Position -> Position -> Position
reflectionPointOnRightWall roomSize earPosition soundPosition =
    let
        earXPositionFromRightWall :: Float
        earXPositionFromRightWall =
            Size.width roomSize - Position.x earPosition

        relativeXDistanceFromRightWall :: Float
        relativeXDistanceFromRightWall =
            earXPositionFromRightWall
                / (earXPositionFromRightWall + (Size.length roomSize - Position.x soundPosition))
    in
    ( Size.width roomSize
    , Position.y earPosition
        + relativeXDistanceFromRightWall
        * (Position.y soundPosition - Position.y earPosition)
    , Position.z earPosition 
        + relativeXDistanceFromRightWall 
        * (Position.z soundPosition - Position.z earPosition)
    )
        |> Position.fromCoords


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
        <| Position.distanceBetween earPosition positionOnFarWall
            + Position.distanceBetween soundPosition positionOnFarWall



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
        |> Position.fromCoords


directDelay :: Position -> Position -> Mono -> Mono
directDelay earPosition soundPosition =
    delayAndDecay 
        <| Position.distanceBetween earPosition soundPosition


delayAndDecay :: Float -> Mono -> Mono
delayAndDecay distance =
    Mono.delay 
        (calcDelay distance) 
        <. Mono.setVolume (Volume (1 / (distance ^ 2)))


calcDelay :: Float -> Duration
calcDelay distance =
    (round (distance / Constants.speedOfSound))
        |> Duration