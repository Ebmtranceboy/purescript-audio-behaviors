module FRP.Event.MIDI
  ( MIDI(..)
  , MIDIAccess(..)
  , MIDIEvent(..)
  , MIDIEventInTime(..)
  , getMidi
  , disposeMidi
  , withMidi
  , midiAccess
  ) where

import Prelude
import Control.Promise (Promise)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Foldable (traverse_)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object as O
import Web.Event.Event as WE
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.Internal.FFI (unsafeReadProtoTagged)

data MIDIEvent
  = NoteOff Int Int Int
  | NoteOn Int Int Int
  | Polytouch Int Int Int
  | ControlChange Int Int Int
  | ProgramChange Int Int
  | Aftertouch Int Int
  | Pitchwheel Int Int

type MIDIEventInTime
  = { timeStamp :: Number
    , event :: MIDIEvent
    }

foreign import data MIDIAccess :: Type

foreign import data MIDIMessageEvent :: Type

foreign import midiAccess :: Effect (Promise MIDIAccess)

foreign import toTargetMap :: MIDIAccess -> Effect (O.Object EventTarget)

foreign import toMIDIEvent_ ::
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> MIDIEvent) ->
  (Int -> Int -> MIDIEvent) ->
  (Int -> Int -> MIDIEvent) ->
  Maybe MIDIEvent ->
  (MIDIEvent -> Maybe MIDIEvent) ->
  ArrayBuffer ->
  Effect (Maybe MIDIEvent)

foreign import getData_ :: (Maybe ArrayBuffer) -> (ArrayBuffer -> Maybe ArrayBuffer) -> MIDIMessageEvent -> Effect (Maybe ArrayBuffer)

foreign import getTimeStamp_ :: (Maybe Number) -> (Number -> Maybe Number) -> MIDIMessageEvent -> Effect (Maybe Number)

getData :: MIDIMessageEvent -> Effect (Maybe ArrayBuffer)
getData = getData_ Nothing Just

getTimeStamp :: MIDIMessageEvent -> Effect (Maybe Number)
getTimeStamp = getTimeStamp_ Nothing Just

newtype MIDI
  = MIDI
  { midi :: Ref.Ref (Map String (List MIDIEventInTime))
  , dispose :: Effect Unit
  }

toMIDIEvent :: ArrayBuffer -> Effect (Maybe MIDIEvent)
toMIDIEvent =
  toMIDIEvent_
    NoteOff
    NoteOn
    Polytouch
    ControlChange
    ProgramChange
    Aftertouch
    Pitchwheel
    Nothing
    Just

fromEvent :: WE.Event -> Maybe MIDIMessageEvent
fromEvent = unsafeReadProtoTagged "MIDIMessageEvent"

getMidi :: MIDIAccess -> Effect MIDI
getMidi midiAccess_ = do
  targetMap <- toTargetMap midiAccess_ >>= pure <<< M.fromFoldable <<< (O.toUnfoldable :: O.Object EventTarget -> List (Tuple String EventTarget))
  midi <- Ref.new M.empty
  let
    makeListener inputName =
      eventListener \e -> do
        fromEvent e
          # traverse_ \me -> do
              data__ <- getData me
              timeStamp__ <- getTimeStamp me
              midiEvent__ <- maybe (pure Nothing) toMIDIEvent data__
              let
                toAdd_ =
                  ( do
                      timeStamp_ <- timeStamp__
                      midiEvent_ <- midiEvent__
                      pure
                        { timeStamp: timeStamp_
                        , event: midiEvent_
                        }
                  )
              case toAdd_ of
                Nothing -> pure unit
                Just toAdd ->
                  ( do
                      _ <-
                        Ref.modify
                          ( \mmap ->
                              M.union
                                ( M.singleton inputName
                                    ( toAdd
                                        : ( fromMaybe Nil
                                              $ M.lookup inputName mmap
                                          )
                                    )
                                )
                                mmap
                          )
                          midi
                      pure unit
                  )
  listeners <-
    sequence
      $ M.mapMaybeWithKey
          ( \k v ->
              Just
                ( do
                    listener <- makeListener k
                    _ <-
                      addEventListener
                        (wrap "midimessage")
                        listener
                        false
                        v
                    pure (Tuple listener v)
                )
          )
          targetMap
  let
    dispose = do
      _ <-
        sequence
          $ M.mapMaybeWithKey
              ( \k (Tuple l v) ->
                  Just
                    ( removeEventListener
                        (wrap "midimessage")
                        l
                        false
                        v
                    )
              )
              listeners
      pure unit
  pure (MIDI { midi, dispose })

disposeMidi :: MIDI -> Effect Unit
disposeMidi (MIDI { dispose }) = dispose

-- | Create an event which also returns the current state of MIDI.
withMidi ::
  forall a.
  MIDI ->
  Event a ->
  Event { value :: a, midi :: Map String (List MIDIEventInTime) }
withMidi (MIDI { midi }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          midi_ <- Ref.read midi
          k { value, midi: midi_ }
