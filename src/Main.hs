module Main where

import qualified Synth.Event as Event
import qualified Synth.JACK as JACK

import Control.Concurrent
import Control.Monad.Trans.Class (lift)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Sound.JACK
import Data.IORef
import Data.Word

import qualified Data.IntMap.Strict as IntMap

import qualified GI.Gtk as Gtk


-- Setup GUI, Event Network and JACK
main :: IO ()
main = do
    _ <- Gtk.init Nothing

    handleExceptions $ do
        client  <- newClientDefault "Haskell-Synth"
        input   <- newPort client "input"
        output  <- newPort client "output"
        rate    <- lift $ getSampleRate client

        (midiEvent, runMidiEvent) <- lift $ newAddHandler
        wave <- lift $ newIORef (pure 0)

        ticks <- lift $ newIORef 0

        let notes = IntMap.empty :: IntMap.IntMap (Double, Double, Word64)

        lift $ actuate =<< compile (Event.network midiEvent notes wave rate)
        withProcess client
            (JACK.process input output ticks wave runMidiEvent) $ do
                _ <- lift (forkOS Gtk.main)
                activate client
                lift waitForBreak
                deactivate client
