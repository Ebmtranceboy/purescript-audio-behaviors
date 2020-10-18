module FRP.Behavior.Audio.Example.Periodic where

import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D1)
import Data.Vec ((+>), empty)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain, gain', makePeriodicWave, periodicOsc, runInBrowser, speaker')
import Foreign.Object as O
import Math (pi, sin)
import Type.Klank.Dev (Klank, klank, PeriodicWaves)

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

periodicWaves :: PeriodicWaves
periodicWaves ctx _ res rej = do
  pw <- makePeriodicWave ctx (0.5 +> 0.25 +> 0.1 +> empty) (0.2 +> 0.1 +> 0.01 +> empty)
  res $ O.singleton "smooth" pw

main :: Klank
main =
  klank
    { periodicWaves = periodicWaves
    , run = runInBrowser scene
    }
    

