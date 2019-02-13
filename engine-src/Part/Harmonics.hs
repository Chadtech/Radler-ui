{-# LANGUAGE OverloadedStrings #-}


module Part.Harmonics
    ( Model
    , makeFlags
    , toMono
    , Error
    , throw
    ) where


import Flow
import Prelude.Extra
    
import Audio.Mono (Mono)
import qualified Audio.Mono as Mono
import qualified Control.Monad as CM
import qualified Data.Either.Extra as Either
import Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Parse (parse)
import qualified Parse
import qualified Part.Osc as Osc
import Part.Volume (Volume(Volume))
import qualified Part.Volume as Volume
import qualified Freq


-- TYPES --


data Model
    = Model (List Harmonic)
    deriving (Eq)


data Harmonic 
    = Harmonic
        { _harmonic :: Float
        , volume :: Volume
        }
        deriving (Eq)


-- HELPERS --


makeFlags :: Parse.Fields Text -> Either Error (Osc.Flags Model)
makeFlags fields =
    case Parse.get "harmonics" fields of
        Just harmonicsText ->
            harmonicsText
                |> T.splitOn ","
                |> List.map parseHarmonic
                |> CM.sequence
                |> Either.mapRight
                    (Osc.Flags toMono fields <. Model)

        Nothing ->
            Left NoHarmonicsField


parseHarmonic :: Text -> Either Error Harmonic
parseHarmonic harmonicText =
    case Parse.fromDelimitedText Parse.float harmonicText of
        Right harmonicsFields ->
            case 
                ( Parse.get "vol" harmonicsFields
                , Parse.get "harmonic" harmonicsFields
                )
            of
                (Just vol, Just harmonic) ->
                    Harmonic harmonic (Volume vol)
                        |> Right


                (Nothing, _) ->
                    Left NoVolume

                (_, Nothing) ->
                    Left NoHarmonic


        Left error ->
            Left <| HarmonicInvalid error


toMono :: Model -> Osc.Note -> Mono
toMono (Model harmonics) note = 
    harmonics
        |> List.map (toHarmonicMono note)
        |> Mono.mixMany


toHarmonicMono :: Osc.Note -> Harmonic -> Mono
toHarmonicMono note harmonic =
    Mono.sin 
        (Freq.map ((*) (_harmonic harmonic)) (Osc.freq note))
        (Osc.duration note)
        |> Mono.setVolume 
            (Volume.multiply (volume harmonic) (Osc.volume note))
        |> Mono.declip


-- ERROR --


data Error 
    = NoHarmonicsField
    | HarmonicInvalid Text
    | NoVolume
    | NoHarmonic


throw :: Error -> Text
throw error =
    case error of
        NoHarmonicsField ->
            "Part details does not contain a harmonics field. \
            \ It should look like this -> \"harmonics(harmonic=2 vol=0.5, harmonic=3vol=0.2)\""

        HarmonicInvalid _ ->
            "This particular harmonic didnt have the shape. I \
            \expected. It should look like this -> \"harmonic=2 vol=0.5\""

        NoVolume ->
            "This particular harmonic didnt have a volume field. I \
            \expected. It should look like this -> \"harmonic=2 vol=0.5\""

        NoHarmonic ->
            "This particular harmonic didnt have a harmonic field. I \
            \expected. It should look like this -> \"harmonic=2 vol=0.5\""
