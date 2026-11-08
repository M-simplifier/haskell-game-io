module GameIO.Platform
  ( Window
  , WindowConfig(..)
  , WindowEvent(..)
  , TimedWindowEvent(..)
  , KeyMessage(..)
  , PointerButton(..)
  , PlatformError(..)
  , withWindow
  , withWindowVisibility
  , nativeWindowHandle
  , windowFramebufferSize
  , pollWindowEvents
  , pollWindowEventsStamped
  , resizeWindow
  , setWindowMinimumSize
  , setPointerCapture
  , windowPointerCaptured
  , setPointerVisible
  ) where

import Foreign.Ptr (Ptr)
import GameIO.Platform.Win32

-- | Borrow the Win32 HWND on its owning thread, within the window scope.
nativeWindowHandle :: Window -> IO (Ptr ())
nativeWindowHandle window = assertWindowThread window >> pure (snd (windowHandles window))
