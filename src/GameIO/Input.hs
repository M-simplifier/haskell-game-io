module GameIO.Input
  ( module GameIO.Input.Event
  , InputTimeline
  , initialTimeline
  , TimelineError (..)
  , acceptEvents
  , takeThrough
  , InputState
  , initialInput
  , keyHeld
  , virtualKeyHeld
  , pointerHeld
  , pointerPosition
  , hasFocus
  , CancelReason (..)
  , KeyEdge (..)
  , InputChange (..)
  , reduceInput
  ) where

import Data.Char (chr)
import Data.Foldable (foldlM, toList)
import Data.Foldable qualified as Foldable
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word16, Word32, Word64)
import GameIO.Input.Event

data InputTimeline = InputTimeline !(Maybe HostStamp) !(Maybe Word64) !(Seq TimedWindowEvent)
  deriving (Eq, Show)

data TimelineError
  = ReorderedEvent !HostStamp !HostStamp
  | EventAfterClosedTick !Word64 !HostStamp
  | ReversedTick !Word64 !Word64
  deriving (Eq, Show)

initialTimeline :: InputTimeline
initialTimeline = InputTimeline Nothing Nothing Seq.empty

-- | Supply all events the application accepted before closing a tick.
-- The caller owns any policy for events that arrive after their tick closed.
acceptEvents :: [TimedWindowEvent] -> InputTimeline -> Either TimelineError InputTimeline
acceptEvents events timeline = foldlM append timeline events
  where
    append (InputTimeline latest closed pending) event
      | Just preceding <- latest, stamp <= preceding = Left (ReorderedEvent preceding stamp)
      | Just tick <- closed, windowEventTimeNSec event <= tick = Left (EventAfterClosedTick tick stamp)
      | otherwise = Right (InputTimeline (Just stamp) closed (pending |> event))
      where stamp = eventStamp event

-- | The tick owns events at or before its boundary. Equal timestamps retain
-- their original order, and advancing the queue never synthesizes repeats.
takeThrough :: Word64 -> InputTimeline -> Either TimelineError ([TimedWindowEvent], InputTimeline)
takeThrough boundary (InputTimeline latest closed pending)
  | Just previous <- closed, boundary < previous = Left (ReversedTick previous boundary)
  | otherwise =
      let (ready, waiting) = Seq.spanl ((<= boundary) . windowEventTimeNSec) pending
      in Right (toList ready, InputTimeline latest (Just boundary) waiting)

data PhysicalKey = PhysicalKey !Word32 !Bool | LogicalKey !Word32
  deriving (Eq, Ord, Show)

data InputState = InputState
  { inputFocused :: !Bool
  , pressedKeys :: !(Map.Map PhysicalKey Word32)
  , pressedButtons :: !(Set.Set PointerButton)
  , pendingSurrogate :: !(Maybe Word16)
  , lastPointer :: !(Maybe (Int, Int))
  }
  deriving (Eq, Show)

initialInput :: InputState
initialInput = InputState False Map.empty Set.empty Nothing Nothing

keyIdentity :: KeyMessage -> PhysicalKey
keyIdentity message
  | keyScanCode message == 0 = LogicalKey (keyVirtual message)
  | otherwise = PhysicalKey (keyScanCode message) (keyExtended message)

keyHeld :: InputState -> KeyMessage -> Bool
keyHeld state = flip Map.member (pressedKeys state) . keyIdentity

virtualKeyHeld :: InputState -> Word32 -> Bool
virtualKeyHeld state code = any (== code) (Map.elems (pressedKeys state))

pointerHeld :: InputState -> PointerButton -> Bool
pointerHeld state = flip Set.member (pressedButtons state)

pointerPosition :: InputState -> Maybe (Int, Int)
pointerPosition = lastPointer

hasFocus :: InputState -> Bool
hasFocus = inputFocused

data CancelReason = FocusLost | PointerCaptureLost deriving (Eq, Show)
data KeyEdge = KeyPressed | KeyReleased | KeyAutoRepeat deriving (Eq, Show)

data InputChange
  = InputKey !KeyEdge !KeyMessage
  | InputText !Char
  | InputComposition !String
  | InputCompositionEnded
  | InputFocus !Bool
  | InputCancel !CancelReason
  | InputPointer !Int !Int
  | InputDelta !Int !Int
  | InputButton !PointerButton !Bool !Int !Int
  | InputWheel !Int !Int !Int !Int
  | InputResize !Int !Int
  | InputClose
  deriving (Eq, Show)

-- | Observe key edges and held state separately. A down and up in one tick
-- still yield two ordered edges, even when the final held set is empty.
reduceInput :: InputState -> [TimedWindowEvent] -> (InputState, [(HostStamp, InputChange)])
reduceInput state events =
  let (end, changes) = Foldable.foldl' observe (state, []) events
  in (end, reverse changes)
  where
    observe (current, collected) event =
      let (next, changes) = reduce current (windowEvent event)
      in (next, Foldable.foldl' (\acc change -> (eventStamp event, change) : acc) collected changes)

reduce :: InputState -> WindowEvent -> (InputState, [InputChange])
reduce state event = case event of
  WindowFocusChanged True -> (state {inputFocused = True}, [InputFocus True])
  WindowFocusChanged False -> (initialInput {lastPointer = lastPointer state}, [InputFocus False, InputCancel FocusLost])
  WindowPointerCaptureLost -> (state {pressedButtons = Set.empty}, [InputCancel PointerCaptureLost])
  WindowResized width height -> keep (InputResize width height)
  CloseRequested -> keep InputClose
  _ | not (inputFocused state) -> (state, [])
  WindowKeyChanged message
    | not (keyDown message) && Map.member identity keys ->
        (state {pressedKeys = Map.delete identity keys}, [InputKey KeyReleased message])
    | keyDown message && keyRepeated message && Map.member identity keys -> keep (InputKey KeyAutoRepeat message)
    | keyDown message && not (keyRepeated message) && not (Map.member identity keys) ->
        (state {pressedKeys = Map.insert identity (keyVirtual message) keys}, [InputKey KeyPressed message])
    | otherwise -> (state, [])
    where identity = keyIdentity message
  WindowTextCodeUnit code ->
    let (pending, characters) = utf16 (pendingSurrogate state) code
    in (state {pendingSurrogate = pending}, map InputText characters)
  WindowComposition composition -> keep (InputComposition composition)
  WindowCompositionEnded -> keep InputCompositionEnded
  WindowPointerMoved x y -> (state {lastPointer = Just (x, y)}, [InputPointer x y])
  WindowPointerDelta x y -> keep (InputDelta x y)
  WindowPointerWheel x y dx dy -> (state {lastPointer = Just (x, y)}, [InputWheel x y dx dy])
  WindowPointerButton button down x y ->
    let held = (if down then Set.insert else Set.delete) button (pressedButtons state)
    in (state {pressedButtons = held, lastPointer = Just (x, y)}, [InputButton button down x y])
  where
    keys = pressedKeys state
    keep change = (state, [change])

utf16 :: Maybe Word16 -> Word16 -> (Maybe Word16, [Char])
utf16 previous unit
  | 0xd800 <= unit && unit <= 0xdbff = (Just unit, unfinished)
  | 0xdc00 <= unit && unit <= 0xdfff =
      (Nothing, [maybe replacement paired previous])
  | otherwise = (Nothing, unfinished ++ [chr (fromIntegral unit)])
  where
    replacement = '\xfffd'
    unfinished = maybe [] (const [replacement]) previous
    paired hi = chr (0x10000 + (fromIntegral hi - 0xd800) * 1024 + fromIntegral unit - 0xdc00)
