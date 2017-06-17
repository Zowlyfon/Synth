module Synth.JACK ( process ) where

import qualified Synth.Wave as Wave

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Exception.Synchronous (ExceptionalT)
import Reactive.Banana.Frameworks
import Sound.JACK
import Sound.JACK.Audio (Sample, getBufferArray)
import Sound.JACK.Exception
import Sound.MIDI
import Data.Array.Storable
import Data.Either
import Data.IORef
import Data.Word

import qualified Sound.JACK.Audio as JAudio
import qualified Sound.JACK.MIDI as JMIDI

-- Jack process function, handles reading and writing data from JACK
process :: ThrowsErrno e =>
           JMIDI.Port Input ->
           JAudio.Port Output ->
           IORef Word64 ->
           IORef (Wave.Wave Sample) ->
           Handler (MidiMessage, Word64) ->
           NFrames -> ExceptionalT e IO ()
process input output ticks sound runEvent nf@(NFrames n) = do
    oarr <- lift $ getBufferArray output nf
    tstart <- lift $ readIORef ticks

    midiEvs <- fmap (decodeMidi1 . JMIDI.rawEventBuffer)
           <$> JMIDI.readRawEventsFromPort input nf

    lift $ mapM_ runEvent (map (\x -> (x, tstart)) (rights midiEvs))

    forM_ (nframesIndices nf) $ \i@(NFrames t) -> do
        wave <- lift $ readIORef sound
        let s = Wave.runWave wave (tstart + fromIntegral t)
        lift $ writeArray oarr i s
    lift $ modifyIORef' ticks (+ fromIntegral n)
