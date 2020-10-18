module FRP.Behavior.Audio.Example.Periodic where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import Data.Vec ((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioUnit, defaultExporter, gain, gain', makePeriodicWave, periodicOsc, runInBrowser, speaker')
import Foreign.Object as O
import Math (pi, sin)

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  let
    rad = pi * time
  in
    pure
      $ speaker'
          ( gain (0.2 + 0.15 * sin (0.2 * rad))
              ( (gain' 0.1 $ periodicOsc "smooth" (1238.0 + (10.0 * sin (30.3 * rad))))
                  :| (gain' 0.12 $ periodicOsc "smooth" (1235.0 + (10.0 * sin (31.7 * rad))))
                  : (gain' 0.05 $ periodicOsc "smooth" (1241.0 + (10.0 * sin (20.0 * rad))))
                  : Nil
              )
          )

main :: AudioContext -> Effect (Effect Unit)
main ctx = do
  pw <- makePeriodicWave ctx (0.5 +> 0.25 +> 0.1 +> empty) (0.2 +> 0.1 +> 0.01 +> empty)
  runInBrowser scene unit
    ctx
    { msBetweenSamples: 20
    , msBetweenPings: 15
    , fastforwardLowerBound: 0.025
    , rewindUpperBound: 0.15
    , initialOffset: 0.1
    }
    { periodicWaves: O.singleton "smooth" pw
    , floatArrays: O.empty
    , microphones: O.empty
    , tracks: O.empty
    , buffers: O.empty
    }
    { canvases: O.empty
    }
    defaultExporter

