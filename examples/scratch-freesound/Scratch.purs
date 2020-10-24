module FRP.Behavior.Audio.Example.Scratch  where

import Prelude
import Control.Promise (toAffE)
import Effect(Effect)
import Effect.Class(liftEffect)
import Effect.Aff (launchAff_)
import Foreign.Object as O
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext,AudioUnit, defaultExporter,decodeAudioDataFromUri, gain',  loopBuf, runInBrowser, speaker')
import Math (pi, sin)

scene :: Number -> Behavior (AudioUnit D1)
scene time = let
      rad = pi * time
    in
      pure $ speaker' (gain' 0.2 (loopBuf "scratch" (2.0 + sin rad) 0.0 0.0))

main :: AudioContext -> Effect Unit
main ctx =  do
    let buf = toAffE $ decodeAudioDataFromUri ctx "https://freesound.org/data/previews/71/71853_1062668-lq.mp3"
    launchAff_ do
      b <- buf
      liftEffect $ runInBrowser scene unit
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
                , buffers: O.singleton "scratch" b
                }
                { canvases: O.empty
                }
                defaultExporter
 
