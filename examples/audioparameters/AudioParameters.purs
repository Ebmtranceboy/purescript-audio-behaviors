module FRP.Behavior.Audio.Example.AudioParameters where

import Prelude
import Data.Array (head, last, range, span)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit,gain', gainT, runInBrowser, sinOsc, speaker', defaultExporter)
import Foreign.Object as O

pwf0 :: Array (Tuple Number Number)
pwf0 =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + 4.0 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.05 0.7, Tuple 0.15 0.08 ]
        )
        (range 0 400)

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 66.66667 Hz

split :: ∀ t12 t13. Ord t12 ⇒ t12 → Array (Tuple t12 t13) → { init ∷ Array (Tuple t12 t13), rest ∷ Array (Tuple t12 t13) }
split s p = span ((s >= _) <<< fst) p

gn :: Number → Array (Tuple Number Number) → AudioParameter Number
gn s p =
  let
    ht = split s p
  in
    let
      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init
    in
      let
        right = fromMaybe (Tuple 101.0 0.0) $ head ht.rest
      in
        -- if we are in a control cycle with a peak or trough
        -- we lock to that
        -- otherwise, we interpolate
        if (fst right - s) < kr then
          AudioParameter { param: (snd right), timeOffset: (fst right - s) }
        else
          let
            m = (snd right - snd left) / (fst right - fst left)
          in
            let
              b = (snd right - (m * fst right))
            in
              AudioParameter { param: (m * s + b), timeOffset: 0.0 }

scene :: Number -> Behavior (AudioUnit D1)
scene t =
  pure
    ( speaker'
        $ gainT (gn t pwf0)
            ( (gain' 0.1 $ sinOsc 450.0)
                :| (gain' 0.5 $ sinOsc 220.0)
                : (gain' 0.25 $ sinOsc 690.0)
                : (gain' 0.1 $ sinOsc 910.0)
                : Nil
            )
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

