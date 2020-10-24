module FRP.Behavior.Audio.Example.HelloWorld where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext,AudioUnit, gain', runInBrowser, sinOsc, speaker, defaultExporter)
import Effect (Effect)
import Foreign.Object as O
import Math (pi, sin)

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  let
    rad = pi * time
  in
    pure
      $ speaker
          ( (gain' 0.1 $ sinOsc (440.0 + (10.0 * sin (2.3 * rad))))
              :| (gain' 0.25 $ sinOsc (235.0 + (10.0 * sin (1.7 * rad))))
              : (gain' 0.2 $ sinOsc (337.0 + (10.0 * sin rad)))
              : (gain' 0.1 $ sinOsc (530.0 + (19.0 * (5.0 * sin rad))))
              : Nil
          )

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

