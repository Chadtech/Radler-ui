module Audio.Position
    ( position
    ) where


import Position (Position)
import qualified Position
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import Audio.Stereo (Stereo)
import qualified Audio.Stereo as Stereo


positionMono :: Position -> Mono -> Stereo
positionMono position mono =
    


