module FRP.Behavior.Audio.Example.Mp3Play where
-- New to PureScript? Check out https://www.purescript.org/ for learning resources!
-- To learn more about FRP and the behavior pattern, make sure to check out:
-- " https://github.com/paf31/purescript-behaviors
-- " https://github.com/mikesol/purescript-audio-behaviors
import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D2)
import Foreign.Object as O
import Data.Vec((+>), empty)
import Effect(Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit,AudioContext,defaultExporter, gain', 
  runInBrowser, dup1, panner, merger,
  sinOsc, play, speaker,
  makeAudioTrack)

scene :: Number -> Behavior (AudioUnit D2)
scene =
  const
    $ pure
        ( dup1
            ( (gain' 0.1 $ sinOsc 349.0)
                + (gain' 0.1 $ sinOsc 220.0)
            ) \mono ->
            speaker
              $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                    :| (gain' 0.5 $ (play "forest"))
                    : Nil
                )
        )

main :: AudioContext -> Effect (Effect Unit)
main ctx = do
 forest <- makeAudioTrack "https://freesound.org/data/previews/530/530415_1648170-lq.mp3"  -- 71853_1062668-lq.mp3" -- "
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
    , tracks: O.singleton "forest" forest
    , buffers: O.empty
    }
    { canvases: O.empty
    }
    defaultExporter

