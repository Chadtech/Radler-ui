module Mono.Fade
    ( in_
    , out
    ) where

import Flow
import Prelude.Extra

import Mono (Mono)
import qualified Mono
import Timing (Timing)
import qualified Timing


out :: Timing -> Mono -> Mono
out timing mono =
    let
        linear :: Mono -> Mono
        linear mono_ =
            let
                lengthOfMono :: Float
                lengthOfMono =
                    mono_
                        |> Mono.length
                        |> toFloat


                divideByLength :: Int -> Float -> Float
                divideByLength index sample =
                    sample * (1 - (toFloat index / lengthOfMono))

            in
            Mono.indexedMap 
                divideByLength
                mono_
    in
    case timing of
        Timing.Linear ->
            linear mono

        Timing.EaseIn ->
            Mono.subtract
                (out Timing.EaseOut mono)
                mono

        Timing.EaseOut ->
            mono
                |> linear
                |> linear

        Timing.EaseInOut ->
            let
                (firstHalf, secondHalf) =
                    Mono.splitAt 
                        (div (Mono.length mono) 2) 
                        mono
            in
            Mono.append
                (out Timing.EaseIn firstHalf)
                (out Timing.EaseOut secondHalf)


in_ :: Timing -> Mono -> Mono
in_ timing mono =
    let 
        linear :: Mono -> Mono
        linear mono_ =
            let
                lengthOfMono :: Float
                lengthOfMono =
                    mono_
                        |> Mono.length
                        |> toFloat


                divideByLength :: Int -> Float -> Float
                divideByLength index sample =
                    sample * (toFloat index / lengthOfMono)

            in
            Mono.indexedMap
                divideByLength
                mono_
    in
    case timing of
        Timing.Linear ->
            linear mono

        Timing.EaseIn ->
            mono 
                |> linear 
                |> linear

        Timing.EaseOut ->
            Mono.subtract
                (in_ Timing.EaseIn mono)
                mono

        Timing.EaseInOut ->
            let
                (firstHalf, secondHalf) =
                    Mono.splitAt 
                        (div (Mono.length mono) 2) 
                        mono
            in
            Mono.append
                (in_ Timing.EaseIn firstHalf)
                (in_ Timing.EaseOut secondHalf)
