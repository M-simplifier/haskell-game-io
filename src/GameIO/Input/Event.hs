module GameIO.Input.Event
  ( WindowEvent (..)
  , KeyMessage (..)
  , PointerButton (..)
  , TimedWindowEvent (..)
  , HostStamp (..)
  , eventStamp
  ) where

import Data.Word (Word16, Word32, Word64)

data PointerButton
  = PointerLeft | PointerMiddle | PointerRight | PointerX1 | PointerX2
  deriving (Eq, Ord, Show)

data KeyMessage = KeyMessage
  { keyVirtual :: !Word32
  , keyScanCode :: !Word32
  , keyExtended :: !Bool
  , keyDown :: !Bool
  , keyRepeated :: !Bool
  } deriving (Eq, Show)

-- | Pointer positions use client pixels. Wheel deltas are the signed native
-- units, including fractions of a detent; text units are UTF-16.
data WindowEvent
  = CloseRequested
  | WindowResized !Int !Int
  | WindowFocusChanged !Bool
  | WindowKeyChanged !KeyMessage
  | WindowTextCodeUnit !Word16
  | WindowComposition !String
  | WindowCompositionEnded
  | WindowPointerMoved !Int !Int
  | WindowPointerDelta !Int !Int
  | WindowPointerButton !PointerButton !Bool !Int !Int
  | WindowPointerWheel !Int !Int !Int !Int
  | WindowPointerCaptureLost
  deriving (Eq, Show)

data TimedWindowEvent = TimedWindowEvent
  { windowEventTimeNSec :: !Word64
  , windowEventSequence :: !Word64
  , windowEvent :: !WindowEvent
  } deriving (Eq, Show)

-- | The sequence breaks ties within one monotonic clock reading.
data HostStamp = HostStamp !Word64 !Word64 deriving (Eq, Ord, Show)

eventStamp :: TimedWindowEvent -> HostStamp
eventStamp item = HostStamp (windowEventTimeNSec item) (windowEventSequence item)
