module FRP.Behavior.Audio.Example.Onset where
-- New to PureScript? Check out https://www.purescript.org/ for learning resources!
-- To learn more about FRP and the behavior pattern, make sure to check out:
-- " https://github.com/paf31/purescript-behaviors
-- " https://github.com/mikesol/purescript-audio-behaviors
import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Typelevel.Num(D2)
import Data.Vec((+>), empty)
import Effect (Effect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Mouse (buttons)
import FRP.Event.Mouse (Mouse, getMouse)
import Data.Array (span, last, head, range)
import Data.Tuple(Tuple(..), fst, snd)
import Foreign.Object as O
import Data.Maybe(Maybe(..), fromMaybe, maybe)
import Data.Int(toNumber)
import Data.Set(isEmpty)
import FRP.Behavior.Audio ( AudioContext, gain', runInBrowser_, play,
  makeAudioTrack, sinOsc, speaker, gainT', IAudioUnit(..),
  AudioParameter(..), defaultExporter,
  dup1, merger, panner)
import Math (pi, sin)

pwf :: Array (Tuple Number Number)
pwf =
  join
    $ map
        ( \i ->
            map
              ( \(Tuple f s) ->
                  Tuple (f + 0.11 * toNumber i) s
              )
              [ Tuple 0.0 0.0, Tuple 0.02 0.7, Tuple 0.06 0.2 ]
        )
        (range 0 400)

kr = 20.0 / 1000.0 :: Number -- the control rate in seconds, or 50 Hz

initialOnset = { onset: Nothing } :: { onset :: Maybe Number }

scene ::
  Mouse ->
  { onset :: Maybe Number  } ->
  Number ->
  Behavior (IAudioUnit D2 { onset :: Maybe Number  })
scene mouse acc@{ onset } time = f time <$> click
  where
  split s = span ((s >= _) <<< fst) pwf

  gn s =
    let
      ht = split s

      left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

      right = fromMaybe (Tuple 201.0 0.0) $ head ht.rest
    in
      if (fst right - s) < kr then
        AudioParameter { param: (snd right), timeOffset: (fst right - s) }
      else
        let
          m = (snd right - snd left) / (fst right - fst left)

          b = (snd right - (m * fst right))
        in
          AudioParameter { param: (m * s + b), timeOffset: 0.0 }

  f s cl =
    IAudioUnit
      ( dup1
          ( (gain' 0.2 $ sinOsc (110.0 + (3.0 * sin (0.5 * rad))))
              + (gain' 0.1 (gainT' (gn s) $ sinOsc 440.0))
              + (gain' 0.1 $ sinOsc (220.0 + (if cl then (50.0 + maybe 0.0 (\t -> 10.0 * (s - t)) stTime) else 0.0)))
          ) \mono ->
          speaker
            $ ( (panner (-0.5) (merger (mono +> mono +> empty)))
                  :| (gain' 0.5 $ (play "forest"))
                  : Nil
              )
      )
      (acc { onset = stTime })
    where
    rad = pi * s

    stTime = case Tuple onset cl of
      (Tuple Nothing true) -> Just s
      (Tuple (Just y) true) -> Just y
      (Tuple _ false) -> Nothing

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse
 

main :: AudioContext -> Effect (Effect Unit)
main ctx = do
  forest <- makeAudioTrack "https://freesound.org/data/previews/530/530415_1648170-lq.mp3"
  runInBrowser_  
       ( do
            mouse <- getMouse
            pure $ scene mouse
        ) 
    initialOnset
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

 
