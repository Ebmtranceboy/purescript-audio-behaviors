module FRP.Behavior.Audio.Example.DrumSequence  where

import Prelude
import Control.Promise (toAffE)
import Data.Array (head, last, span)
import Data.Int (toNumber)
import Data.List (List(..), range)
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D2)
import Effect (Effect)
import Effect.Class(liftEffect)
import Effect.Aff (Aff, ParAff, launchAff_, parallel, sequential)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit, BrowserAudioBuffer, decodeAudioDataFromUri, defaultExporter, gain, gainT_', playBufT_, runInBrowser, speaker')  
import Foreign.Object as O

kr = 20.0 / 1000.0 :: Number

pwf :: Number -> Number -> Array (Number /\ Number)
pwf s dur =
  [ 0.0 /\ 1.0
  , (s + dur - 0.05) /\ 1.0
  , (s + dur - 0.02) /\ 0.0
  ]

split :: Number -> Array (Number /\ Number) -> { init :: Array (Number /\ Number), rest :: Array (Number /\ Number) } 
split s p = span ((s >= _) <<< fst) p

gn :: Number -> Array (Number /\ Number) -> AudioParameter Number
gn s p =
  let
    ht = split s p

    left = fromMaybe (0.0 /\ 0.0) $ last ht.init

    right = fromMaybe (101.0 /\ 0.0) $ head ht.rest
  in
    if (fst right - s) < kr then
      AudioParameter
        { param: (snd right)
        , timeOffset: (fst right - s)
        }
    else
      let
        m = (snd right - snd left) / (fst right - fst left)

        b = (snd right - (m * fst right))
      in
        AudioParameter { param: (m * s + b), timeOffset: 0.0 }

withinWindow :: Number -> Number -> Number -> Number -> Boolean
withinWindow t krate s dur = t + krate > s && t < (s + dur + krate)

trigger :: String -> Number -> Number -> Number -> List (AudioUnit D2)
trigger name t s dur =
  ( if withinWindow t kr s dur then
      pure
        $ gainT_' (name <> "g_g" <> (show s)) (gn t (pwf s dur))
            ( playBufT_ (name <> "_" <> (show s)) name
                (AudioParameter { param: 1.0, timeOffset: max (s - t) 0.0 })
            )
    else
      Nil
  )

tempo = 60.0 / 90.0 :: Number

tempoize :: Number -> Number
tempoize n = tempo * n

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    ( speaker'
        ( gain 1.0
            ( zero
                :| ( ( (trigger "wavbvkery/Kicks/22.wav" time (tempoize 0.0) 2.0)
                        <> (trigger "wavbvkery/Kicks/22.wav" time (tempoize 2.0) 2.0)
                        <> (trigger "wavbvkery/Kicks/22.wav" time (tempoize 6.0) 2.0)
                        <> (trigger "wavbvkery/Kicks/22.wav" time (tempoize 8.0) 2.0)
                    )
                      <> ( (trigger "wavbvkery/Snares/09.wav" time (tempoize 1.0) 2.0)
                            <> (trigger "wavbvkery/Snares/09.wav" time (tempoize 3.0) 2.0)
                            <> (trigger "wavbvkery/Snares/09.wav" time (tempoize 5.0) 2.0)
                            <> (trigger "wavbvkery/Snares/09.wav" time (tempoize 7.0) 2.0)
                        )
                      <> (join (map (\i -> (trigger "wavbvkery/Hats/02.wav" time (tempoize 0.0 + (toNumber i * 0.5)) 2.0)) (range 0 15)))
                  )
            )
        )
    )

dl :: AudioContext -> O.Object BrowserAudioBuffer -> Array (Tuple String (ParAff BrowserAudioBuffer))
dl ctx prev =
  ( join
      $ map
          ( \i ->
              if O.member i prev then
                []
              else
                [ Tuple i
                    $ ( parallel
                          $ toAffE
                              ( decodeAudioDataFromUri
                                  ctx
                                  $ "https://sound.klank.dev/"
                                  <> i
                              )
                      )
                ]
          )
          filez
  )

buffers :: AudioContext -> O.Object BrowserAudioBuffer -> Aff (O.Object BrowserAudioBuffer)
buffers ctx prev =
          O.union prev <$> (sequential
            $ sequence
                (O.fromFoldable $ dl ctx prev))

filez :: Array String
filez =
  [ "buchla/Tabla_9_bip.wav"
  , "wavbvkery/Hats/02.wav"
  , "wavbvkery/Snares/22.wav"
  , "wavbvkery/Snares/09.wav"
  , "wavbvkery/Kicks/22.wav"

  ]

main :: AudioContext -> Effect Unit
main ctx =  do
    launchAff_ do
      b <- buffers ctx O.empty
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
                , buffers: b
                }
                { canvases: O.empty
                }
                defaultExporter
 
