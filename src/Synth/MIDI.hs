module Synth.MIDI ( isNote
                  , runNotes ) where

import Sound.MIDI

import qualified Data.IntMap.Strict as IntMap

-- Return True if MIDI Event is a keypress
isNote :: MidiMessage -> Bool
isNote (ChannelVoice NoteOn{}) = True
isNote (ChannelVoice NoteOff{}) = True
isNote _ = False

-- Convert from a MIDI pitch to a frequency
getFrequency :: Integral a => a -> Double
getFrequency n = 2 ** (((fromIntegral n) - 69) / 12) * 440 

-- Convert velocity from Integral to Amplitude
scaleVelocity :: Integral a => a -> Double
scaleVelocity v = fromIntegral v / 127

-- Take MIDI notes and insert/delete them into an IntMap
runNotes :: Integral a =>
            MidiMessage -> 
            a -> 
            IntMap.IntMap (Double, Double, a) -> 
            IntMap.IntMap (Double, Double, a)
runNotes (ChannelVoice (NoteOn _ p v)) t notes = 
    IntMap.insert (fromIntegral (getPitch p)) 
                  (getFrequency (getPitch p), scaleVelocity (getVelocity v), t) 
                  notes
runNotes (ChannelVoice (NoteOff _ p _)) _ notes = 
    IntMap.delete (fromIntegral (getPitch p)) notes
runNotes _ _ _ = error "Internal Error: runNotes"
