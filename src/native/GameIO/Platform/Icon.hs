{-# LANGUAGE ForeignFunctionInterface #-}

module GameIO.Platform.Icon (withWindowIcon) where

import Control.Exception (bracket, throwIO)
import Control.Monad (void)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.String (CWString, withCWString)
import Foreign.Ptr (IntPtr(..), Ptr, WordPtr(..), intPtrToPtr, nullPtr, ptrToIntPtr)
import GameIO.Platform.Win32 (PlatformError(..), Window, assertWindowThread, windowHandles)

type Handle = Ptr ()

foreign import ccall safe "LoadImageW"
  loadImage :: Handle -> CWString -> Word32 -> Int32 -> Int32 -> Word32 -> IO Handle
foreign import ccall unsafe "DestroyIcon"
  destroyIcon :: Handle -> IO Int32
foreign import ccall unsafe "GetLastError"
  getLastError :: IO Word32
foreign import ccall unsafe "GetDpiForWindow"
  getDpi :: Handle -> IO Word32
foreign import ccall unsafe "GetSystemMetricsForDpi"
  getMetric :: Int32 -> Word32 -> IO Int32
foreign import ccall safe "SendMessageW"
  sendMessage :: Handle -> Word32 -> WordPtr -> IntPtr -> IO IntPtr

-- | The supplied .ico is owned by the caller. Windows' icon handles are
-- installed for this scope, and previous icons are restored before release.
withWindowIcon :: Window -> FilePath -> IO a -> IO a
withWindowIcon window path action = do
  assertWindowThread window
  let (_, handle) = windowHandles window
  dpi <- getDpi handle
  bracket (load dpi 11 12) (void . destroyIcon) $ \large ->
    bracket (load dpi 49 50) (void . destroyIcon) $ \small ->
      bracket (swap handle 1 large) (void . swap handle 1) $ \_ ->
        bracket (swap handle 0 small) (void . swap handle 0) (const action)
  where
    load dpi horizontal vertical = withCWString path $ \name -> do
      width <- getMetric horizontal dpi
      height <- getMetric vertical dpi
      icon <- loadImage nullPtr name 1 width height 0x10
      if icon == nullPtr then getLastError >>= throwIO . Win32Failed "LoadImageW (icon)" else pure icon
    swap handle size icon = intPtrToPtr <$> sendMessage handle 0x0080 size (ptrToIntPtr icon)
