module FRP.Behavior.Audio.Example.MIDI where

import Prelude
import Control.Promise (toAffE)
import Data.Array (fold, head)
import Data.Int (toNumber)
import Data.List (List(..), fromFoldable, sort, (:))
import Data.List as DL
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Set (isEmpty)
import Data.Set as DS
import Data.Tuple (snd)
import Data.Typelevel.Num (D1)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (joinFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioUnit, AudioInfo, EngineInfo, Exporter, IAudioUnit(..), RunInBrowserIAudioUnit, VisualInfo, gain, gain_', runInBrowser, sawtoothOsc_, speaker', defaultExporter)   
import FRP.Behavior.MIDI (midi)
import FRP.Behavior.Mouse (buttons)
import FRP.Event.MIDI (MIDI, MIDIEvent(..), MIDIEventInTime, getMidi, midiAccess)
import FRP.Event.Mouse (Mouse, getMouse)
import Foreign.Object as O
import Math (pi, pow, sin)

gen = sawtoothOsc_ :: String -> Number -> AudioUnit D1

type OffOn
  = { off :: DS.Set Int, on :: DS.Set Int }

afterTimeStampIfExists :: Number -> Number -> Maybe Number -> Boolean
afterTimeStampIfExists _ _ Nothing = true

afterTimeStampIfExists time midiTime (Just correction) = (midiTime - correction) < time

synth :: Number -> Maybe Number -> M.Map String (List MIDIEventInTime) -> DS.Set Int
synth time timeCorrection m =
  fromMaybe
    DS.empty
    ( do
        h <- head (M.toUnfoldable m)
        pure $ (go { off: DS.empty, on: DS.empty } (snd h)).on
    )
  where
  go :: OffOn -> List MIDIEventInTime -> OffOn
  go acc Nil = acc

  go acc@{ off, on } ({ timeStamp, event: (NoteOn _ n _) } : rest)
    | afterTimeStampIfExists time (timeStamp / 1000.0) timeCorrection =
      go
        { off: DS.singleton n <> off
        , on: if not (DS.member n off) then DS.singleton n <> on else on
        }
        rest
    | otherwise = go acc rest

  go acc@{ off, on } ({ timeStamp, event: (NoteOff _ n _) } : rest)
    | afterTimeStampIfExists (time - 0.1) (timeStamp / 1000.0) timeCorrection =
      go
        { off: DS.singleton n <> off
        , on
        }
        rest
    | otherwise = go acc rest

  go acc (_ : rest) = go acc rest

midi2cps :: Int -> Number
midi2cps i = 440.0 * (2.0 `pow` (toNumber (spy "midi" i - 69) / 12.0))

type MIDIAccumulator r
  = { syncWithAudioClock :: Maybe Number | r }
  
initialClock = { syncWithAudioClock: Just 0.0 } ::  { syncWithAudioClock :: Maybe Number }

scene :: forall r. Mouse -> MIDI -> MIDIAccumulator r -> Number -> Behavior (IAudioUnit D1 (MIDIAccumulator r))
scene mouse midiIn acc time = f <$> click <*> (midi midiIn)
  where
  rad = pi * time

  f cl md =
    IAudioUnit
      ( speaker'
          ( gain 0.1
              ( zero
                  :| ( map (\i -> gain_' ("gain" <> show i) 0.4 (gen ("sinOsc" <> show i) ((midi2cps i) + (if (not cl) then 0.0 else 10.0 * sin (7.0 * rad))))) (fromFoldable $ synth time newCorrection md)
                    )
              )
          )
      )
      (acc { syncWithAudioClock = newCorrection })
    where
    newCorrection = case acc.syncWithAudioClock of
      Nothing ->
        map
          (\t -> (t / 1000.0) - time)
          (DL.head (sort (map _.timeStamp (fold (M.values md)))))
      Just y -> Just y

  click :: Behavior Boolean
  click = map (not <<< isEmpty) $ buttons mouse

runWithMidi ::
  forall microphone track buffer floatArray periodicWave r env.
  (Mouse -> MIDI -> MIDIAccumulator r -> Number -> Behavior (IAudioUnit D1 (MIDIAccumulator r))) ->
  (MIDIAccumulator r) ->
  AudioContext ->
  EngineInfo ->
  AudioInfo (O.Object microphone) (O.Object track) (O.Object buffer) (O.Object floatArray) (O.Object periodicWave) ->
  VisualInfo ->
  Exporter env ->
  Effect (Effect Unit)
runWithMidi callback ac ctx ei ai vi ex = do
  fiber <-
    launchAff do
      macc <- toAffE midiAccess
      md <- liftEffect $ getMidi macc
      mouse <- liftEffect $ getMouse
      liftEffect
        $ ( runInBrowser ::
               RunInBrowserIAudioUnit (MIDIAccumulator r) D1 env
          )
            (callback mouse md)
            ac
            ctx
            ei
            ai
            vi
            ex
  ( do
      let
        rest = joinFiber fiber
      pure
        $ launchAff_ do
            o <- rest
            liftEffect $ o
  )

main :: AudioContext -> Effect (Effect Unit)
main ctx = do
  runWithMidi 
    scene
    initialClock
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

 

