module FRP.Behavior.Audio.Example.AudioWorklet where

import Prelude
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext,AudioUnit, audioWorkletAggregator, audioWorkletGenerator, audioWorkletProcessor, gain', runInBrowser, sinOsc, speaker', defaultExporter)
import Foreign.Object as O
import Math (pi, sin)

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    $ speaker'
        ( audioWorkletGenerator
            "white-noise-processor"
            (O.singleton "customGain" $ 0.05 + (0.05 * sin (0.1 * rad)))
        )
  where
  rad = time * pi

scene1 :: Number -> Behavior (AudioUnit D1)
scene1 time =
  pure
    $ speaker'
        ( audioWorkletProcessor
            "gain-processor"
            (O.singleton "customGain" $ 0.05 + (0.05 * sin (1.0 * rad)))
            (sinOsc 220.0)
        )
  where
  rad = time * pi

scene1p :: Number -> Behavior (AudioUnit D1)
scene1p time =
  pure
    $ speaker'
        ( gain' (0.05 + (0.05 * sin (1.0 * rad)))
            (sinOsc 220.0)
        )
  where
  rad = time * pi

scene2 :: Number -> Behavior (AudioUnit D1)
scene2 time =
  pure
    $ speaker'
        ( gain' 0.2
            ( audioWorkletAggregator
                "add-processor"
                (O.empty)
                (sinOsc 220.0)
                (sinOsc 440.0)
            )
        )
  where
  rad = time * pi

scene2p :: Number -> Behavior (AudioUnit D1)
scene2p time =
  pure
    $ speaker'
        ( gain' 0.2
            ( (sinOsc 220.0)
                + (sinOsc 440.0)
            )
        )
  where
  rad = time * pi

main :: AudioContext -> Effect (Effect Unit)
main ctx = do
 runInBrowser scene unit
    ctx
    { msBetweenSamples: 20
    , msBetweenPings: 15
    , fastforwardLowerBound: 0.025
    , rewindUpperBound: 0.15
    , initialOffset: 0.1
    , doWebAudio: true
    }
    { periodicWaves: O.empty
    , floatArrays: O.empty
    , microphones: O.empty
    , tracks: O.empty
    , buffers: O.empty
    }
    { canvases: O.empty
    }
    defaultExporter

