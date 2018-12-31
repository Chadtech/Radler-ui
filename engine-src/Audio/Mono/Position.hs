module Audio.Mono.Position
    ( positionMono
    ) where


import Position (Position)
import qualified Position
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Audio.Stereo (Stereo)
import qualified Audio.Stereo as Stereo
import Room (Room)
import qualified Room 


positionMono :: Room -> Position -> Mono -> Stereo
positionMono room position mono =
    Stereo.fromMono mono



