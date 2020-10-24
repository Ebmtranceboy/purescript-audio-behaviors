module FRP.Behavior.Audio.Example.Bach where

import Prelude
import Control.Promise (toAffE)
import Data.Array (foldl, head, last, length, mapWithIndex, range, span, take, zipWith)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D2)
import Effect (Effect)
import Effect.Aff (Aff, ParAff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Random (random)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit, BrowserAudioBuffer, decodeAudioDataFromUri, gainT_', playBufT_, runInBrowser_, speaker,defaultExporter)
import Foreign.Object as O

kr = 20.0 / 1000.0 :: Number

pwf :: Number -> Number -> Array (Tuple Number Number)
pwf s dur =
  [ Tuple 0.0 1.0
  , Tuple (s + dur - 0.05) 1.0
  , Tuple (s + dur - 0.02) 0.0
  ]

split :: Number -> Array (Number /\ Number) -> { init :: Array (Number /\ Number), rest :: Array (Number /\ Number) } 
split s p = span ((s >= _) <<< fst) p

gn :: Number -> Array (Number /\ Number) -> AudioParameter Number
gn s p =
  let
    ht = split s p

    left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

    right = fromMaybe (Tuple 101.0 0.0) $ head ht.rest
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

harp :: String -> Number -> Number -> Number -> Boolean -> List (AudioUnit D2)
harp name t s dur doRelease =
  ( if t + kr > s && t < (s + dur + kr) then
      pure
        $ gainT_' (name <> "g_g" <> (show s)) (gn t (pwf s dur))
            ( playBufT_ (name <> "_" <> (show s)) name
                (AudioParameter { param: 1.0, timeOffset: max (s - t) 0.0 })
            )
    else
      Nil
  )
    <> ( if doRelease && t >= (s - 0.2 + dur + kr) && t < (s - 0.2 + dur + kr + 3.0) then
          pure
            $ playBufT_ (name <> "_" <> (show s <> "r")) (name <> "r")
                (AudioParameter { param: 1.0, timeOffset: max (s - t) 0.0 })
        else
          Nil
      )

tempo = 0.096 :: Number

stac = 3.8 :: Number

stdm :: Int -> Array Int -> Array PVD
stdm n a =
  mapWithIndex
    ( \i x ->
        PVD x (toNumber (i + n * 6))
          (tempo * stac)
          false
    )
    a

chord :: Int -> Int -> Number -> Array Int -> Array PVD
chord n b l a =
  map
    ( \x ->
        PVD x (toNumber (b + n * 6))
          (tempo * l)
          true
    )
    a

twom :: Int -> (Tuple Int Int) -> Array PVD
twom n (Tuple a b) =
  [ PVD a (toNumber $ n * 6)
      (tempo * 5.5)
      true
  , PVD b (toNumber $ n * 6 + 4)
      (tempo * 3.2)
      false
  ]

m30ToM31 :: Array PVD
m30ToM31 =
  [ PVD 47 (toNumber $ 30 * 6)
      (tempo * 4.5)
      true
  , PVD 59 (toNumber $ 30 * 6 + 4)
      (tempo * 5.5)
      true
  , PVD 57 (toNumber $ 30 * 6 + 8)
      (tempo * 4.5)
      true
  , PVD 56 (toNumber $ 30 * 6 + 10)
      (tempo * 4.5)
      true
  ]

m61 :: Array PVD
m61 =
  [ PVD 31 (toNumber $ 61 * 6)
      (tempo * 4.5)
      true
  , PVD 30 (toNumber $ 61 * 6 + 2)
      (tempo * 4.5)
      true
  , PVD 28 (toNumber $ 61 * 6 + 4)
      (tempo * 4.5)
      true
  ]

endOfPhrase :: Int -> EOP -> Array PVD
endOfPhrase n (EOP a b c d e f) =
  [ PVD a (toNumber $ n * 6)
      (tempo * 4.5)
      true
  , PVD b (toNumber $ n * 6 + 4)
      (tempo * 5.5)
      true
  , PVD c (toNumber $ n * 6 + 8)
      (tempo * stac)
      false
  , PVD d (toNumber $ n * 6 + 9)
      (tempo * stac)
      false
  , PVD e (toNumber $ n * 6 + 10)
      (tempo * stac)
      false
  , PVD f (toNumber $ n * 6 + 11)
      (tempo * stac)
      false
  ]

data Seq8
  = Seq8 Int Int Int Int Int

seq8' :: Int -> Seq8 -> Array PVD
seq8' n (Seq8 a b c d e) =
  [ PVD a (toNumber $ n * 6)
      (tempo * 4.5)
      true
  , PVD b (toNumber (n * 6) + 1.9)
      (tempo * 3.4)
      true
  , PVD c (toNumber $ n * 6 + 4)
      (tempo * 6.5)
      true
  , PVD d (toNumber $ n * 6 + 8)
      (tempo * 4.5)
      true
  , PVD e (toNumber $ n * 6 + 10)
      (tempo * 4.5)
      true
  ]

seq8 :: Int -> Seq8 -> Array PVD
seq8 n (Seq8 a b c d e) =
  [ PVD a (toNumber $ n * 6)
      (tempo * 4.5)
      true
  , PVD b (toNumber $ n * 6 + 2)
      (tempo * 4.5)
      true
  , PVD c (toNumber $ n * 6 + 4)
      (tempo * 6.5)
      true
  , PVD d (toNumber $ n * 6 + 8)
      (tempo * 4.5)
      true
  , PVD e (toNumber $ n * 6 + 10)
      (tempo * 4.5)
      true
  ]

hocket0 :: Int -> Hocket0 -> Number -> Array PVD
hocket0 n (Hocket0 r0 r1 l0 l1 l2) p =
  [ PVD r0 (toNumber $ n * 6)
      (tempo * 4.5)
      true
  , PVD r1 (toNumber $ n * 6 + 1)
      (tempo * stac)
      true
  , PVD r1 (toNumber $ n * 6 + 3)
      (tempo * stac)
      true
  , PVD r1 (toNumber $ n * 6 + 5)
      (tempo * stac)
      true
  , PVD l0 (toNumber $ n * 6)
      (tempo * p)
      true
  , PVD l1 (toNumber $ n * 6 + 2)
      (tempo * stac)
      true
  , PVD l2 (toNumber $ n * 6 + 4)
      (tempo * stac)
      true
  ]

hocket1 :: Int -> Hocket1 -> Array PVD
hocket1 n (Hocket1 r0 r1 r2 r3 r4 r5 r6 l0 l1 l2 l3 l4 l5) =
  [ PVD r0 (toNumber $ n * 6)
      (tempo * 4.5)
      true
  , PVD r1 (toNumber $ n * 6 + 1)
      (tempo * stac)
      true
  , PVD r2 (toNumber $ n * 6 + 3)
      (tempo * stac)
      true
  , PVD r3 (toNumber $ n * 6 + 5)
      (tempo * stac)
      true
  , PVD r4 (toNumber $ n * 6 + 7)
      (tempo * stac)
      true
  , PVD r5 (toNumber $ n * 6 + 9)
      (tempo * stac)
      true
  , PVD r6 (toNumber $ n * 6 + 11)
      (tempo * stac)
      true
  , PVD l0 (toNumber $ n * 6)
      (tempo * 6.5)
      true
  , PVD l1 (toNumber $ n * 6 + 2)
      (tempo * stac)
      true
  , PVD l2 (toNumber $ n * 6 + 4)
      (tempo * stac)
      true
  , PVD l3 (toNumber $ n * 6 + 6)
      (tempo * 6.5)
      true
  , PVD l4 (toNumber $ n * 6 + 8)
      (tempo * stac)
      true
  , PVD l5 (toNumber $ n * 6 + 10)
      (tempo * stac)
      true
  ]

hocket2 :: Int -> Hocket2 -> Array PVD
hocket2 n (Hocket2 r1 r2 r3 r4 r5 r6 l0 l1 l2 l3 l4 l5) =
  [ PVD r1 (toNumber $ n * 6 + 1)
      (tempo * stac)
      true
  , PVD r2 (toNumber $ n * 6 + 3)
      (tempo * stac)
      true
  , PVD r3 (toNumber $ n * 6 + 5)
      (tempo * stac)
      true
  , PVD r4 (toNumber $ n * 6 + 7)
      (tempo * stac)
      true
  , PVD r5 (toNumber $ n * 6 + 9)
      (tempo * stac)
      true
  , PVD r6 (toNumber $ n * 6 + 11)
      (tempo * stac)
      true
  , PVD l0 (toNumber $ n * 6)
      (tempo * 6.5)
      true
  , PVD l1 (toNumber $ n * 6 + 2)
      (tempo * stac)
      true
  , PVD l2 (toNumber $ n * 6 + 4)
      (tempo * stac)
      true
  , PVD l3 (toNumber $ n * 6 + 6)
      (tempo * 6.5)
      true
  , PVD l4 (toNumber $ n * 6 + 8)
      (tempo * stac)
      true
  , PVD l5 (toNumber $ n * 6 + 10)
      (tempo * stac)
      true
  ]

threeDots :: Int -> ThreeDots -> Array PVD
threeDots n (ThreeDots l0 l1 l2) =
  [ PVD l0 (toNumber $ n * 6)
      (tempo * 6.5)
      true
  , PVD l1 (toNumber $ n * 6 + 2)
      (tempo * stac)
      true
  , PVD l2 (toNumber $ n * 6 + 4)
      (tempo * stac)
      true
  ]

data ThreeDots
  = ThreeDots Int Int Int

data Hocket0
  = Hocket0 Int Int Int Int Int

data Hocket1
  = Hocket1 Int Int Int Int Int Int Int Int Int Int Int Int Int

data Hocket2
  = Hocket2 Int Int Int Int Int Int Int Int Int Int Int Int

data EOP
  = EOP Int Int Int Int Int Int

data Transposition
  = M2

tpz :: Int -> Transposition -> Int
tpz i t = case t of
  M2 -> case i of
    0 -> 2
    2 -> 2
    4 -> 1
    5 -> 2
    7 -> 2
    9 -> 2
    11 -> 1
    _ -> 0

harmT :: Int -> Transposition -> Array PVD -> Array PVD
harmT i t = map (\(PVD a b c d) -> (PVD (tpz ((a - i) `mod` 12) t + a) b c d))

piece =
  stdm 0 [ 54, 50, 45, 50, 54, 50 ]
    <> twom 0 (Tuple 26 38)
    <> stdm 1 [ 55, 50, 55, 50, 55, 50 ]
    <> twom 1 (Tuple 28 38)
    <> stdm 2 [ 57, 50, 57, 50, 57, 50 ]
    <> twom 2 (Tuple 30 38)
    <> stdm 3 [ 59, 50, 59, 50, 59, 50 ]
    <> twom 3 (Tuple 31 38)
    <> stdm 4 [ 57, 50, 57, 50, 57, 50 ]
    <> twom 4 (Tuple 30 38)
    <> stdm 5 [ 55, 54, 52, 54, 55, 52 ]
    <> twom 5 (Tuple 28 37)
    <> stdm 6 [ 54, 52, 50, 52, 54, 50 ]
    <> endOfPhrase 6 (EOP 26 38 37 35 33 35)
    <> stdm 7 [ 52, 54, 52, 50, 49, 47 ]
    <> stdm 8 [ 37, 33, 28, 33, 37, 33 ]
    <> twom 8 (Tuple 45 57)
    <> stdm 9 [ 38, 33, 38, 33, 38, 33 ]
    <> twom 9 (Tuple 47 57)
    <> stdm 10 [ 40, 33, 40, 33, 40, 33 ]
    <> twom 10 (Tuple 49 57)
    <> stdm 11 [ 42, 33, 42, 33, 42, 33 ]
    <> twom 11 (Tuple 50 57)
    <> stdm 12 [ 40, 33, 40, 33, 40, 33 ]
    <> twom 12 (Tuple 49 57)
    <> stdm 13 [ 38, 37, 35, 37, 38, 35 ]
    <> twom 13 (Tuple 47 56)
    <> stdm 14 [ 37, 35, 33, 35, 37, 33 ]
    <> endOfPhrase 14 (EOP 45 57 55 54 52 54)
    <> stdm 15 [ 35, 36, 35, 33, 31, 30 ]
    <> harmT 2 M2
        ( stdm 16 [ 54, 50, 45, 50, 54, 50 ]
            <> twom 16 (Tuple 26 38)
            <> stdm 17 [ 55, 50, 55, 50, 55, 50 ]
            <> twom 17 (Tuple 28 38)
            <> stdm 18 [ 57, 50, 57, 50, 57, 50 ]
            <> twom 18 (Tuple 30 38)
        )
    <> stdm 19 [ 60, 52, 60, 52, 60, 52 ]
    <> harmT 2 M2
        ( twom 19 (Tuple 31 38)
            <> stdm 20 [ 57, 50, 57, 50, 57, 50 ]
            <> twom 20 (Tuple 30 38)
            <> stdm 21 [ 55, 54, 52, 54, 55, 52 ]
        )
    <> twom 21 (Tuple 30 39)
    <> harmT 2 M2
        ( stdm 22 [ 54, 52, 50, 52, 54, 50 ]
            <> endOfPhrase 22 (EOP 26 38 37 35 33 35)
            <> stdm 23 [ 52, 54, 52, 50, 49, 47 ]
        )
    <> harmT 2 M2
        ( stdm 24 [ 37, 33, 28, 33, 37, 33 ]
            <> twom 24 (Tuple 45 57)
            <> stdm 25 [ 38, 33, 38, 33, 38, 33 ]
            <> twom 25 (Tuple 47 57)
            <> stdm 26 [ 40, 33, 40, 33, 40, 33 ]
            <> twom 26 (Tuple 49 57)
            <> stdm 27 [ 42, 33, 42, 33, 42, 33 ]
            <> twom 27 (Tuple 50 57)
            <> stdm 28 [ 40, 33, 40, 33, 40, 33 ]
            <> twom 28 (Tuple 49 57)
            <> stdm 29 [ 38, 37, 35, 37, 38, 35 ]
        )
    <> twom 29 (Tuple 49 58)
    <> m30ToM31
    <> seq8' 32 (Seq8 57 45 57 55 54)
    <> stdmm 30 38
    <> stdmM 31 41
    <> stdm 32 [ 42, 41, 42, 44, 42, 40 ]
    <> stdmM 33 39
    <> seq8 34 (Seq8 40 28 40 38 37)
    <> seq8 36 (Seq8 38 26 38 37 35)
    <> stdmm 34 55
    <> stdmM 35 58
    <> stdm 36 [ 59, 58, 59, 61, 59, 57 ]
    <> stdmM 37 56
    <> seq8 38 (Seq8 57 45 57 55 54)
    <> seq8 40 (Seq8 55 43 55 54 52)
    <> stdmM 38 37
    <> stdmM 39 39
    <> stdm 40 [ 40, 39, 40, 42, 40, 38 ]
    <> stdmM 41 37
    <> seq8 42 (Seq8 38 26 38 36 35)
    <> seq8 44 (Seq8 36 24 36 35 33)
    <> stdmM 42 54
    <> stdmM 43 56
    <> stdm 44 [ 57, 56, 57, 59, 57, 55 ]
    <> stdmM 45 54
    <> map (\(PVD a b c d) -> (PVD (a - 2) b c d))
        ( stdm 46 [ 37, 33, 28, 33, 37, 33 ]
            <> twom 46 (Tuple 45 57)
            <> stdm 47 [ 38, 33, 38, 33, 38, 33 ]
            <> twom 47 (Tuple 47 57)
            <> stdm 48 [ 40, 33, 40, 33, 40, 33 ]
            <> twom 48 (Tuple 49 57)
            <> stdm 49 [ 42, 33, 42, 33, 42, 33 ]
            <> twom 49 (Tuple 50 57)
            <> stdm 50 [ 40, 33, 40, 33, 40, 33 ]
            <> twom 50 (Tuple 49 57)
            <> stdm 51 [ 38, 37, 35, 37, 38, 35 ]
            <> twom 51 (Tuple 47 56)
            <> stdm 52 [ 37, 35, 33, 35, 37, 33 ]
            <> endOfPhrase 52 (EOP 45 57 56 54 52 54)
        )
    <> stdm 53 [ 33, 35, 33, 31, 30, 28 ]
    <> stdm 54 [ 54, 50, 45, 50, 54, 50 ]
    <> twom 54 (Tuple 26 38)
    <> stdm 55 [ 55, 50, 55, 50, 55, 50 ]
    <> twom 55 (Tuple 28 38)
    <> stdm 56 [ 57, 50, 57, 50, 57, 50 ]
    <> twom 56 (Tuple 30 38)
    <> stdm 57 [ 59, 50, 59, 50, 59, 50 ]
    <> twom 57 (Tuple 31 38)
    <> stdm 58 [ 57, 50, 57, 50, 57, 50 ]
    <> twom 58 (Tuple 30 38)
    <> stdm 59 [ 55, 54, 52, 54, 55, 52 ]
    <> twom 59 (Tuple 28 37)
    <> stdm 60 [ 54, 52, 50, 52, 54, 50 ]
    <> twom 60 (Tuple 26 38)
    <> stdm 61 [ 52, 54, 52, 50, 49, 47 ]
    <> m61
    <> hocket0 62 (Hocket0 49 45 33 43 40) 6.5
    <> hocket0 63 (Hocket0 49 45 33 43 40) 6.3
    <> hocket0 64 (Hocket0 50 45 33 42 38) 6.8
    <> hocket0 65 (Hocket0 50 45 33 42 38) 6.5
    <> hocket0 66 (Hocket0 55 45 33 40 37) 6.3
    <> hocket0 67 (Hocket0 55 45 33 40 37) 6.1
    <> hocket0 68 (Hocket0 54 45 33 42 38) 6.3
    <> hocket0 69 (Hocket0 54 45 33 42 38) 6.5
    <> hocket0 70 (Hocket0 56 47 33 41 38) 6.8
    <> hocket0 71 (Hocket0 56 47 33 41 38) 6.5
    <> hocket0 72 (Hocket0 57 49 33 40 37) 6.3
    <> hocket0 73 (Hocket0 57 49 31 40 37) 6.1
    <> hocket1 74 (Hocket1 57 50 45 50 54 57 59 30 42 38 33 30 28)
    <> stdm 76 [ 60, 59, 57, 55, 54, 52 ]
    <> stdm 77 [ 54, 55, 57, 60, 59, 57 ]
    <> threeDots 76 (ThreeDots 27 30 35)
    <> threeDots 77 (ThreeDots 39 42 45)
    <> hocket1 78 (Hocket1 59 52 43 47 52 55 57 43 40 35 31 28 26)
    <> map (\(PVD a b c d) -> (PVD (a - 2) b c d))
        ( stdm 80 [ 60, 59, 57, 55, 54, 52 ]
            <> stdm 81 [ 54, 55, 57, 60, 59, 57 ]
            <> threeDots 80 (ThreeDots 27 30 35)
            <> threeDots 81 (ThreeDots 39 42 45)
        )
    <> hocket1 82 (Hocket1 57 50 53 50 46 43 52 41 38 34 31 40 37)
    <> hocket2 84 (Hocket2 45 41 50 46 43 40 29 38 34 31 28 25)
    <> map (\(PVD a b c d) -> PVD (a - 12) b c d)
        ( hocket0 86 (Hocket0 49 45 33 43 40) 6.2
            <> hocket0 87 (Hocket0 49 45 33 43 40) 6.5
            <> hocket0 88 (Hocket0 50 45 33 42 38) 6.3
            <> hocket0 89 (Hocket0 50 45 33 42 38) 6.5
            <> hocket0 90 (Hocket0 55 45 33 40 37) 6.8
            <> hocket0 91 (Hocket0 55 45 33 40 37) 6.1
            <> hocket0 92 (Hocket0 54 45 33 42 38) 6.3
            <> hocket0 93 (Hocket0 54 45 33 42 38) 6.5
            <> hocket0 94 (Hocket0 56 47 33 41 38) 6.3
            <> hocket0 95 (Hocket0 56 47 33 41 38) 6.1
        )
    <> stdm 96 [ 21, 25, 28, 31, 34, 37 ]
    <> stdm 97 [ 38, 41, 44, 47, 50, 53 ]
    <> [ PVD 21 (toNumber $ 98 * 6)
          (tempo * 6.5)
          true
      ]
    <> stdm 98 [ 49, 52, 55, 52, 49, 45 ]
    <> stdm 99 [ 43, 40, 37, 33, 37, 40 ]
    <> [ PVD 33 (toNumber $ 100 * 6)
          (tempo * 6.5)
          true
      ]
    <> stdm 100 [ 42, 45, 50, 45, 42, 38 ]
    <> stdm 101 [ 35, 32, 29, 26, 29, 32 ]
    <>
  [ PVD 21 (toNumber $ (102 - endCursorOffset) * 6 + 1)
      (tempo * 12.0)
      true
  ]
    <> chord (102 - endCursorOffset) 5 7.4 [ 26, 30, 33, 42, 45, 50 ]
    <> chord (102 - endCursorOffset) 11 7.4 [ 28, 31, 33, 40, 45, 49 ]
    <> chord (102 - endCursorOffset) 18 60.0 [ 26, 30, 33, 38, 45, 50 ] ::
    Array PVD

endCursorOffset = 0 :: Int

stdmM :: Int -> Int -> Array PVD
stdmM i s = stdm i [ s, s - 2, s - 4, s - 2, s, s - 4 ]

stdmm :: Int -> Int -> Array PVD
stdmm i s = stdm i [ s, s - 1, s - 3, s - 1, s, s - 3 ]

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

dl :: AudioContext -> O.Object BrowserAudioBuffer -> String -> Array (Tuple String (ParAff BrowserAudioBuffer))
dl ctx prev s =
  (join $ map
    ( \i ->
        if O.member (show i <> s) prev then [] else [Tuple (show i <> s)
          $ ( parallel
                $ toAffE
                    ( decodeAudioDataFromUri
                        ctx
                        $ "https://sound.klank.dev/petit-italien-k2/jeu-1/"
                        <> show i
                        <> s
                        <> ".mp3"
                    )
            )]
    )
    (range 12 63))

buffers :: AudioContext -> O.Object BrowserAudioBuffer -> Aff (O.Object BrowserAudioBuffer)
buffers ctx prev =
          O.union prev <$> (sequential
            $ sequence
                (O.fromFoldable $ dl ctx prev "" <> dl ctx prev "r" ))

main :: AudioContext -> Effect Unit
main ctx =
    launchAff_ do
      bufs <- buffers ctx O.empty
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
                      smaller = map (\a -> Tuple (fst a * 0.005) (snd a * 0.005)) randy
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
 
