module FRP.Behavior.Audio.Example.Piece where

import Prelude
import Control.Promise (toAffE)
import Data.Array (drop, foldl, length, mapWithIndex, range, take, zipWith)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.List as L
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D2)
import Effect (Effect)
import Effect.Aff (Aff, ParAff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Random (random)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit, BrowserAudioBuffer, decodeAudioDataFromUri, playBufT_, runInBrowser_, speaker, defaultExporter)
import Foreign.Object as O

kr = 20.0 / 1000.0 :: Number

harp :: String -> Number -> Number -> Number -> Boolean -> List (AudioUnit D2)
harp name t s dur doRelease =
  ( if t + kr > s && t < (s + dur + kr) then
      pure
        $ playBufT_ (name <> "_" <> (show s)) name
            (AudioParameter { param: 1.0, timeOffset: max (s - t) 0.0 })
    else
      Nil
  )
    <> ( if doRelease && t >= (s + dur + kr) && t < (s + dur + kr + 3.0) then
          pure
            $ playBufT_ (name <> "_" <> (show s <> "r")) (name <> "r")
                (AudioParameter { param: 1.0, timeOffset: max (s - t) 0.0 })
        else
          Nil
      )

tempo = 0.2173 :: Number

tempoB = 0.194 :: Number

mez :: Int -> Array Int -> Array PVD
mez n a =
  mapWithIndex
    ( \i x ->
        let
          moddy = (i `mod` 8)
        in
          PVD x (toNumber (i + n * 16))
            (tempo * (3.0 + (0.5 * (toNumber $ if moddy < 3 then 8 - moddy else 2))))
            true
    )
    (a_ <> a_)
  where
  a_ = a <> drop 2 a

fin = 512.0 :: Number

piece =
  mez 0 [ 37, 41, 44, 49, 53 ]
    <> mez 1 [ 37, 39, 46, 51, 54 ]
    <> mez 2 [ 36, 39, 44, 51, 54 ]
    <> mez 3 [ 37, 41, 44, 49, 53 ]
    <> mez 4 [ 37, 41, 46, 53, 58 ]
    <> mez 5 [ 37, 39, 43, 46, 51 ]
    <> mez 6 [ 36, 39, 44, 51, 56 ]
    <> mez 7 [ 36, 37, 41, 44, 49 ]
    <> mez 8 [ 34, 37, 41, 44, 49 ]
    <> mez 9 [ 27, 34, 39, 43, 49 ]
    <> mez 10 [ 32, 36, 39, 44, 48 ]
    <> mez 11 [ 32, 35, 41, 44, 50 ]
    <> mez 12 [ 30, 34, 39, 46, 51 ]
    <> mez 13 [ 30, 33, 39, 42, 48 ]
    <> mez 14 [ 29, 32, 37, 44, 49 ]
    <> mez 15 [ 29, 30, 34, 37, 42 ]
    <> mez 16 [ 27, 30, 34, 37, 42 ]
    <> mez 17 [ 20, 27, 32, 36, 42 ]
    <> mez 18 [ 25, 29, 32, 37, 41 ]
    <> mez 19 [ 25, 32, 35, 37, 41 ]
    <> mez 20 [ 18, 30, 34, 37, 41 ]
    <> mez 21 [ 19, 25, 34, 37, 40 ]
    <> mez 22 [ 21, 30, 36, 37, 39 ]
    <> mez 23 [ 20, 30, 32, 36, 39 ]
    <> mez 24 [ 20, 29, 32, 37, 41 ]
    <> mez 25 [ 20, 27, 32, 37, 42 ]
    <> mez 26 [ 20, 27, 32, 36, 42 ]
    <> mez 27 [ 20, 28, 34, 37, 43 ]
    <> mez 28 [ 20, 29, 32, 37, 44 ]
    <> mez 29 [ 20, 27, 32, 37, 42 ]
    <> mez 30 [ 20, 27, 32, 36, 42 ]
    <> mez 31 [ 13, 25, 32, 35, 41 ]
    <> ( map (\(PVD a b c d) -> PVD a (b + fin) c d)
          ( _.acc
              $ foldl
                  ( \{ acc, prev } (PVD a b d dr) ->
                      { acc: acc <> [ PVD a prev d dr ], prev: b + prev
                      }
                  )
                  { acc: [], prev: 0.0 }
                  [ PVD 13 1.0 1.5 true
                  , PVD 25 1.0 1.5 true
                  , PVD 30 1.0 1.5 true
                  , PVD 34 1.0 1.5 true
                  , PVD 37 1.0 1.5 true
                  , PVD 42 1.0 1.5 true
                  , PVD 37 1.0 1.5 true
                  , PVD 34 1.0 1.5 true
                  , PVD 37 1.0 1.5 true
                  , PVD 34 1.0 1.5 true
                  , PVD 30 1.0 1.5 true
                  , PVD 34 1.0 1.5 true
                  , PVD 30 1.1 1.5 true
                  , PVD 27 1.1 1.5 true
                  , PVD 30 1.2 1.5 true
                  , PVD 27 1.6 1.5 true
                  , PVD 13 1.9 6.0 true
                  , PVD 24 1.65 6.0 true
                  , PVD 44 1.4 6.0 false
                  , PVD 48 1.25 6.0 false
                  , PVD 51 1.2 6.0 true
                  , PVD 54 1.1 6.0 true
                  , PVD 51 1.05 6.0 true
                  , PVD 48 1.0 6.0 false
                  , PVD 51 1.05 6.0 false
                  , PVD 48 1.1 6.0 false
                  , PVD 44 1.15 6.0 false
                  , PVD 48 1.2 5.0 false
                  , PVD 39 1.3 4.0 false
                  , PVD 42 1.7 3.0 false
                  , PVD 41 2.0 3.0 false
                  , PVD 39 6.0 3.0 false
                  , PVD 13 0.5 6.0 false
                  , PVD 25 0.2 6.0 false
                  , PVD 41 0.23 6.0 false
                  , PVD 44 0.2 6.0 false
                  , PVD 49 1.0 6.0 false
                  , PVD 39 1.0 6.0 false
                  , PVD 41 1.0 6.0 false
                  ]
          )
      ) ::
    Array PVD

data PVD
  = PVD Int Number Number Boolean

scene :: List PVD -> Number -> Behavior (AudioUnit D2)
scene pc t =
  pure
    ( speaker
        ( zero
            :| ( join
                  ( map
                      ( \(PVD p v d b) ->
                          harp (show (p + 0)) t
                            (tempo * v)
                            d
                            b
                      )
                      pc
                  )
              )
        )
    )

dl :: AudioContext -> String -> Array (Tuple String (ParAff BrowserAudioBuffer))
dl ctx s =
  map
    ( \i ->
        Tuple (show i <> s)
          $ ( parallel
                $ toAffE
                    ( decodeAudioDataFromUri
                        ctx
                        $ "https://sound.klank.dev/petit-italien-k2/jeu-1/"
                        <> show i
                        <> s
                        <> ".mp3"
                    )
            )
    )
    [ 13, 18, 19, 20, 21, 25
    , 27, 28, 29, 30, 31, 32
    , 33, 34, 35, 36, 37, 39
    , 40, 41, 42, 43, 44, 46
    , 48, 49, 50, 51, 53, 54
    , 56, 58] 

buffers :: AudioContext -> Aff (O.Object BrowserAudioBuffer)
buffers ctx =
         (sequential
            $ sequence
                (O.fromFoldable $ dl ctx "" <> dl ctx "r" ))

main :: AudioContext -> Effect Unit
main ctx =
    launchAff_ do
      bufs <- buffers ctx
      liftEffect $ runInBrowser_ do
                        -- a slight jank to liven it up
                        randy <-
                          sequence
                            $ map
                                ( const
                                    $ ( do
                                          r0 <- random
                                          r1 <- random
                                          pure $ Tuple r0 r1
                                      )
                                )
                                (range 0 (length piece - 1))
                        additiveJank <-
                          sequence
                            $ map
                                (const random)
                                (range 0 (length piece - 1))
                        let
                          smaller = map (\a -> Tuple (fst a * 0.015) (snd a * 0.3)) randy
                        let
                          pjanked =
                            zipWith
                              ( \aj (PVD a b d e) ->
                                  ( PVD a
                                      (b + (0.02 * (foldl (+) 0.0 $ take aj additiveJank)))
                                      d
                                      e
                                  )
                              )
                              ( range 0
                                  (length additiveJank - 1)
                              )
                              $ zipWith (\(Tuple r x) (PVD a b d e) -> (PVD a (b + r) (d + x) e)) smaller piece
                        pure $ scene (L.fromFoldable pjanked)
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
                , buffers: bufs
                }
                { canvases: O.empty
                }
                defaultExporter
 
