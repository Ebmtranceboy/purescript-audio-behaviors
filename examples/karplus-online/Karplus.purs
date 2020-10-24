module FRP.Behavior.Audio.Example.Karplus where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio
  ( AudioUnit
  , AudioContext
  , defaultExporter
  , audioWorkletGenerator
  , gain
  , g'add
  , g'delay
  , g'gain
  , graph
  , runInBrowser
  , speaker'
  )
import Foreign.Object as O
import Effect  (Effect)
import Record.Extra (SLProxy(..), SNil)
import Type.Data.Graph (type (:/))
import Data.NonEmpty ((:|))
import Data.List (List(..), (:))
import Math (pi)

pluck :: Number -> Number -> AudioUnit D1
pluck f d =
  ( graph
      { aggregators:
          { out: Tuple g'add (SLProxy :: SLProxy ("combine" :/ SNil))
          , combine: Tuple g'add (SLProxy :: SLProxy ("gain" :/ "mic" :/ SNil))
          , gain: Tuple (g'gain 0.9) (SLProxy :: SLProxy ("del" :/ SNil))
          }
      , processors:
          { del: Tuple (g'delay d) (SProxy :: SProxy "combine")
          }
      , generators:
          { mic:
              audioWorkletGenerator
                "klank-lf-burst"
                (O.union (O.singleton "nsamples" 25.0) (O.singleton "freq" f))
          }
      }
  )

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    $ speaker'
        ( gain 0.5
            ( (pluck 1.0 0.004)
                :| (pluck 1.2 0.003)
                : (pluck 1.3333 0.005)
                : (pluck 1.49 0.006)
                : Nil
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

