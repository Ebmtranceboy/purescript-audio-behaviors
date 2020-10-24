module FRP.Behavior.Audio.Example.Mouse where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.Set (isEmpty)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import Foreign.Object as O
import FRP.Behavior.Audio (AudioContext, AudioUnit, gain', runInBrowser_, sinOsc, speaker',defaultExporter)
import FRP.Behavior.Mouse (buttons, position)
import FRP.Event.Mouse (Mouse, getMouse)

-- scene = const $ pure (speaker' $ (gain' 0.1 $ sinOsc 440.0))
-- map (a -> b) (f a -> f b)
scene :: Mouse -> Number -> Behavior (AudioUnit D1)
scene mouse = const $ f <$> click <*> poz
  where
  f cl pozz = speaker' (gain' (if cl then 0.2 else 0.0) $ sinOsc (maybe 0.1 (\{ y } -> (toNumber y) + 220.0) pozz))

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse

  poz ::
    Behavior
      ( Maybe
          { x :: Int, y :: Int }
      )
  poz = position mouse

main :: AudioContext -> Effect (Effect Unit)
main ctx = do
 runInBrowser_  
       ( do
            mouse <- getMouse
            pure $ scene mouse
        ) 
    unit
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

