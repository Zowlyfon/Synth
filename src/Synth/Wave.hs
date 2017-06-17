{-# LANGUAGE MultiWayIf #-}

module Synth.Wave ( Wave (..)
                  , foldWaves
                  , makeWave
                  , sine
                  , square
                  , sawtooth
                  , triangle ) where

import Data.Word
import Linear.Vector
import Reactive.Banana

import qualified Data.IntMap.Strict as IntMap

-- Define a type for Waves
newtype Wave a = Wave { runWave :: Word64 -> a }

-- Required instances for new type
instance Functor Wave where
    fmap f (Wave k) = Wave $ f . k

instance Applicative Wave where
    pure x = Wave (const x)
    f <*> x = Wave $ \t -> runWave f t (runWave x t)

instance Additive Wave where
    zero = pure 0

-- Convert Ticks to Seconds
sampled :: Fractional a => Int -> Word64 -> (Double -> a) -> Wave a
sampled r t f = Wave $ \s -> f (fromIntegral (s - t)  / fromIntegral r)

-- Wave Functions
sine :: Fractional a => Int -> Word64 -> Double -> Wave a
sine r t f = sampled r t $ \x -> realToFrac (sin (2 * pi * f * x))

square :: Fractional a => Int -> Word64 -> Double -> Wave a
square r t = fmap signum . (sine r t)

sawtooth :: Fractional a => Int -> Word64 -> Double -> Wave a
sawtooth r t f = sampled r t $ \x -> realToFrac (2 * ((x * f) - fromIntegral (floor (0.5 + (x * f)) :: Int)))

triangle :: Fractional a => Int -> Word64 -> Double -> Wave a
triangle r t = fmap (subtract 1 . abs . (* 2)) . (sawtooth r t)

-- Fold multiple waves into a single wave
foldWaves :: Fractional a =>
             Behavior (IntMap.IntMap (Double, Double, Word64)) ->
             Behavior ((Double, Double, Word64) -> Wave a) -> 
             Behavior (Wave a)
foldWaves notes wave = (fmap (\f -> IntMap.foldr (\freq a -> f freq ^+^ a) zero) wave) <*> notes

-- Attack Decay Sustain envelope
ads :: Fractional a => Int -> Word64 -> Double -> Double -> a -> Wave a
ads r t a d s = sampled r t $ \x -> if | x <= a -> realToFrac (x / a)
                                       | a < x && x <= (d + a) ->
                                        let x' = realToFrac (x - a)
                                         in (((s - 1) * x') / realToFrac d) + 1
                                       | otherwise -> s

-- Vector Multiplication
(^*^) :: Fractional a => Wave a -> Wave a -> Wave a
(^*^) = liftA2 (*)

-- Combine Wave functions to produce desired sound
makeWave :: Fractional a =>
            Int ->
            Double ->
            a ->
            a ->
            Double ->
            Double ->
            a ->
            (Int -> Word64 -> Double -> Wave a) ->
            (Int -> Word64 -> Double -> Wave a) -> 
            (Double, Double, Word64) -> Wave a
makeWave r lfo oscAmp1 oscAmp2 a d s osc1 osc2 (freq, v, t) =
    (realToFrac v) *^ ((ads r t a d s) ^*^
    (0.25 *^ sine r t lfo) ^*^
    (0.5 *^ (((0.1 * oscAmp1) *^ osc1 r t freq) ^+^ ((0.1 * oscAmp2) *^ osc2 r t freq))))

