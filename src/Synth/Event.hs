{-# LANGUAGE RecordWildCards #-}

module Synth.Event ( network ) where

import Data.Word
import Data.IORef
import Reactive.Banana
import Reactive.Banana.Frameworks
import Sound.MIDI

import qualified Data.IntMap.Strict as IntMap

import qualified Synth.MIDI as MIDI
import qualified Synth.Wave as Wave
import qualified Synth.GUI as GUI

-- Convert from the GUI.Oscillator type to a Wave function
oscToWave :: Fractional a => 
             GUI.Oscillator -> (Int -> Word64 -> Double -> Wave.Wave a)
oscToWave GUI.Sine = Wave.sine
oscToWave GUI.Square = Wave.square
oscToWave GUI.Saw = Wave.sawtooth
oscToWave GUI.Triangle = Wave.triangle

-- Combine a list of events into a single event for reactimate
anyE :: [Event a] -> Event ()
anyE = foldr (unionWith (\_ _ -> ()) . (() <$)) never

network :: Fractional a => 
           AddHandler (MidiMessage, Word64) -> 
           IntMap.IntMap (Double, Double, Word64) -> 
           IORef (Wave.Wave a) -> 
           Int -> MomentIO ()
network midiH notes ref rate = do
   
    GUI.GUIEvs{..}  <- GUI.guiNetwork

    midiE           <- fromAddHandler midiH
    let midiNotesE = filterE (\(x, _) -> MIDI.isNote x) midiE

    -- Accumilate all Midi note events into a single Behavior
    notesB          <- accumB notes $ fmap (\(m, t) -> MIDI.runNotes m t) midiNotesE

    -- Create Behaviours from GUI Events
    lfoV            <- stepper 5 lfoE
    oscAmp1V        <- stepper 1 oscAmp1E
    oscAmp2V        <- stepper 1 oscAmp2E
    attackV         <- stepper 0.2 attackE
    decayV          <- stepper 0.2 decayE
    sustainV        <- stepper 0.75 sustainE
    osc1V           <- stepper GUI.Sine osc1E
    osc2V           <- stepper GUI.Sine osc2E
        
        
    -- Fold all notes into a wave for JACk
    let wave :: Fractional a => 
                Behavior ((Double, Double, Word64) -> Wave.Wave a)
        wave = Wave.makeWave <$> (pure rate)
                             <*> (realToFrac <$> lfoV)
                             <*> (realToFrac <$> oscAmp1V)
                             <*> (realToFrac <$> oscAmp2V)
                             <*> (realToFrac <$> attackV)
                             <*> (realToFrac <$> decayV)
                             <*> (realToFrac <$> sustainV)
                             <*> (oscToWave <$> osc1V)
                             <*> (oscToWave <$> osc2V)

        foldWavesV = Wave.foldWaves notesB wave

    -- Re-Fold Waves when a relevant Event occurs
    reactimate $ fmap (writeIORef ref) (foldWavesV <@ anyE [ 0 <$ osc1E
                                                           , 0 <$ osc2E
                                                           , 0 <$ midiE
                                                           , lfoE
                                                           , attackE
                                                           , decayE
                                                           , sustainE
                                                           , oscAmp1E
                                                           , oscAmp2E
                                                           ])

