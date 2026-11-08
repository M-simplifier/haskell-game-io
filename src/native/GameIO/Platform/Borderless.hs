{-# LANGUAGE ForeignFunctionInterface #-}

module GameIO.Platform.Borderless (withBorderlessToggle) where

import Control.Exception (finally, mask_, onException, throwIO)
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.), complement)
import Data.Int (Int32)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (IntPtr(..), Ptr, nullPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GameIO.Platform.Win32 (PlatformError(..), Window, assertWindowThread, windowHandles)

type Handle = Ptr ()

data Mode = Windowed | Borderless !IntPtr | Ended

foreign import ccall unsafe "GetWindowLongPtrW" getWindowLong :: Handle -> Int32 -> IO IntPtr
foreign import ccall safe "SetWindowLongPtrW" setWindowLong :: Handle -> Int32 -> IntPtr -> IO IntPtr
foreign import ccall unsafe "GetWindowPlacement" getPlacement :: Handle -> Ptr () -> IO Int32
foreign import ccall safe "SetWindowPlacement" setPlacement :: Handle -> Ptr () -> IO Int32
foreign import ccall unsafe "MonitorFromWindow" nearestMonitor :: Handle -> Word32 -> IO Handle
foreign import ccall unsafe "GetMonitorInfoW" getMonitorInfo :: Handle -> Ptr () -> IO Int32
foreign import ccall safe "SetWindowPos" setWindowPos :: Handle -> Handle -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO Int32
foreign import ccall unsafe "SetLastError" setLastError :: Word32 -> IO ()
foreign import ccall unsafe "GetLastError" getLastError :: IO Word32

-- | Toggle the window's nearest monitor at its desktop resolution. The saved
-- window style and normal/maximized placement belong to this scope.
withBorderlessToggle :: Window -> (IO () -> IO a) -> IO a
withBorderlessToggle window use = allocaBytesAligned placementBytes 4 $ \placement -> do
  assertWindowThread window
  state <- newIORef Windowed
  let (_, handle) = windowHandles window
      restore style = do
        changeStyle handle style
        require "SetWindowPlacement" =<< setPlacement handle placement
        require "SetWindowPos (restore frame)" =<< setWindowPos handle nullPtr 0 0 0 0 (frameFlags .|. 0x0003)
      toggle = mask_ $ do
        assertWindowThread window
        current <- readIORef state
        case current of
          Ended -> throwIO WindowClosed
          Borderless style -> restore style >> writeIORef state Windowed
          Windowed -> do
            style <- getWindowLong handle (-16)
            when (style == 0) (failure "GetWindowLongPtrW (style)")
            fillBytes placement 0 placementBytes
            pokeByteOff placement 0 (fromIntegral placementBytes :: Word32)
            require "GetWindowPlacement" =<< getPlacement handle placement
            (left, top, width, height) <- monitorRect handle
            let popup = (style .&. complement 0x21cf0000) .|. 0x80000000
            (do
                changeStyle handle popup
                require "SetWindowPos (borderless frame)" =<< setWindowPos handle nullPtr left top width height frameFlags
              ) `onException` restore style
            writeIORef state (Borderless style)
      finish = mask_ $ do
        current <- readIORef state
        writeIORef state Ended
        case current of
          Borderless style -> restore style
          _ -> pure ()
  use toggle `finally` finish
  where
    placementBytes = 44
    frameFlags = 0x0034 -- SWP_FRAMECHANGED | SWP_NOACTIVATE | SWP_NOZORDER

monitorRect :: Handle -> IO (Int32, Int32, Int32, Int32)
monitorRect handle = allocaBytesAligned 40 4 $ \info -> do
  monitor <- nearestMonitor handle 2
  when (monitor == nullPtr) (failure "MonitorFromWindow")
  pokeByteOff info 0 (40 :: Word32)
  require "GetMonitorInfoW" =<< getMonitorInfo monitor info
  left <- peekByteOff info 4
  top <- peekByteOff info 8
  right <- peekByteOff info 12
  bottom <- peekByteOff info 16
  pure (left, top, right - left, bottom - top)

changeStyle :: Handle -> IntPtr -> IO ()
changeStyle handle style = do
  setLastError 0
  previous <- setWindowLong handle (-16) style
  when (previous == 0) $ do
    problem <- getLastError
    unless (problem == 0) (throwIO (Win32Failed "SetWindowLongPtrW (style)" problem))

require :: String -> Int32 -> IO ()
require name value = when (value == 0) (failure name)

failure :: String -> IO a
failure name = getLastError >>= throwIO . Win32Failed name
