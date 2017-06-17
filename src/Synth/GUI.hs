{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Synth.GUI ( GUIEvs (..)
                 , Oscillator (..)
                 , guiNetwork ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GI.Gtk

import qualified GI.Gtk as Gtk

-- Data structure which stores GUI events
data GUIEvs = GUIEvs
    { osc1E :: Event Oscillator
    , osc2E :: Event Oscillator
    , oscAmp1E :: Event Double
    , oscAmp2E :: Event Double
    , lfoE :: Event Double
    , attackE :: Event Double
    , decayE :: Event Double
    , sustainE :: Event Double
    }

-- Data structure which stores Oscillator wave types
data Oscillator = Sine | Square | Triangle | Saw deriving (Show, Eq)

-- Helper function which combines a list of Events into a single Event
leftUnion :: [Event a] -> Event a
leftUnion xs = foldl (unionWith (\a _ -> a)) never xs

-- Converts 4 RadioButton bool values into a single Oscillator
selectOsc4 :: Gtk.RadioButton ->
              Gtk.RadioButton ->
              Gtk.RadioButton ->
              Gtk.RadioButton -> MomentIO (Event Oscillator)
selectOsc4 rSine rSquare rSaw rTriangle = do
    rSineE <- (Sine <$) . filterE (==True) <$> (attrE rSine #active)
    rSquareE <- (Square <$) . filterE (==True) <$> (attrE rSquare #active)
    rSawE <- (Saw <$) . filterE (==True) <$> (attrE rSaw #active)
    rTriangleE <- (Triangle <$) . filterE (==True) <$> (attrE rTriangle #active)
    pure $ leftUnion [rSineE, rSquareE, rSawE, rTriangleE]

-- Sets up the GUI Events
guiNetwork :: MomentIO GUIEvs
guiNetwork = do
    b               <- Gtk.builderNew
    _               <- Gtk.builderAddFromFile b "gui.glade"

    w               <- castB b "window" Gtk.Window
    destroyE        <- signalE0 w #destroy

    reactimate $ Gtk.mainQuit <$ destroyE

    oscSine1        <- castB b "oscSine1" Gtk.RadioButton
    oscSquare1      <- castB b "oscSquare1" Gtk.RadioButton
    oscSaw1         <- castB b "oscSaw1" Gtk.RadioButton
    oscTriangle1    <- castB b "oscTriangle1" Gtk.RadioButton

    osc1Ev          <- selectOsc4 oscSine1 oscSquare1 oscSaw1 oscTriangle1

    oscSine2        <- castB b "oscSine2" Gtk.RadioButton
    oscSquare2      <- castB b "oscSquare2" Gtk.RadioButton
    oscSaw2         <- castB b "oscSaw2" Gtk.RadioButton
    oscTriangle2    <- castB b "oscTriangle2" Gtk.RadioButton

    osc2Ev          <- selectOsc4 oscSine2 oscSquare2 oscSaw2 oscTriangle2

    oscAmp1         <- castB b "oscAmpAdj1" Gtk.Adjustment
    oscAmp1Ev       <- attrE oscAmp1 #value

    oscAmp2         <- castB b "oscAmpAdj2" Gtk.Adjustment
    oscAmp2Ev       <- attrE oscAmp2 #value

    lfo             <- castB b "lfoAdj" Gtk.Adjustment
    lfoEv           <- attrE lfo #value

    attack          <- castB b "attackAdj" Gtk.Adjustment
    attackEv        <- attrE attack #value

    decay           <- castB b "decayAdj" Gtk.Adjustment
    decayEv         <- attrE decay #value

    sustain         <- castB b "sustainAdj" Gtk.Adjustment
    sustainEv       <- attrE sustain #value

    #showAll w

    pure $ GUIEvs osc1Ev osc2Ev oscAmp1Ev oscAmp2Ev lfoEv attackEv decayEv sustainEv

