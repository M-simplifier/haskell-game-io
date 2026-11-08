{-# LANGUAGE ForeignFunctionInterface #-}

module GameIO.Platform.Win32
  ( Window
  , WindowConfig(..)
  , WindowEvent(..)
  , TimedWindowEvent(..)
  , KeyMessage(..)
  , PointerButton(..)
  , PlatformError(..)
  , withWindow
  , withWindowVisibility
  , windowFramebufferSize
  , pollWindowEvents
  , pollWindowEventsStamped
  , resizeWindow
  , setWindowMinimumSize
  , setPointerCapture
  , windowPointerCaptured
  , setPointerVisible
  , windowHandles
  , assertWindowThread
  , withWindowRenderer
  , retainNativeWindow
  ) where

import Control.Concurrent (runInBoundThread)
import Control.Exception
  ( Exception, SomeException, bracket, catch, finally, mask, mask_
  , onException, throwIO
  )
import Control.Monad (unless, void, when)
import Data.Char (ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Foreign hiding (void)
import GameIO.Input.Event
import GHC.Clock (getMonotonicTimeNSec)

type Handle = Ptr ()
type WinBool = Int32
type LResult = IntPtr
type WindowProc = Handle -> Word32 -> WordPtr -> IntPtr -> IO LResult

data WindowConfig = WindowConfig
  { windowTitle :: String
  , windowWidth :: Int
  , windowHeight :: Int
  } deriving (Eq, Show)

data PlatformError
  = InvalidWindowSize !Int !Int
  | InvalidWindowTitle
  | WrongWindowThread
  | WindowClosed
  | WindowRendererInUse
  | WindowNativeWorkUnconfirmed
  | Win32Failed !String !Word32
  deriving (Eq, Show)

instance Exception PlatformError

data Inbox = Inbox
  { inboxEvents :: !(IORef (Word64, [TimedWindowEvent]))
  , inboxFailure :: !(IORef (Maybe SomeException))
  , inboxCapture :: !(IORef Bool)
  , inboxVisible :: !(IORef Bool)
  , inboxMinimumSize :: !(IORef (Maybe (Int, Int)))
  }

data Window = Window
  { windowHandle :: !Handle
  , windowInstance :: !Handle
  , windowClass :: !String
  , windowProc :: !(FunPtr WindowProc)
  , windowThread :: !Word32
  , windowInbox :: !Inbox
  , windowLive :: !(IORef Bool)
  , windowClaimed :: !(IORef Bool)
  , windowRetained :: !(IORef Bool)
  }

withWindow :: WindowConfig -> (Window -> IO a) -> IO a
withWindow = withWindowVisibility True

withWindowVisibility :: Bool -> WindowConfig -> (Window -> IO a) -> IO a
withWindowVisibility visible config use = runInBoundThread $
  bracket (setThreadDpiAwarenessContext perMonitorAwareV2) restoreDpi $ \_ ->
    bracket (openWindow visible config) closeWindow use
  where
    restoreDpi previous =
      unless (previous == nullPtr) (void (setThreadDpiAwarenessContext previous))

openWindow :: Bool -> WindowConfig -> IO Window
openWindow visible config = mask_ $ do
  checkExtent (windowWidth config) (windowHeight config)
  when ('\0' `elem` windowTitle config) (throwIO InvalidWindowTitle)
  instanceHandle <- getModuleHandle nullPtr >>= requireHandle "GetModuleHandleW"
  cursor <- loadCursor nullPtr (wordPtrToPtr idcArrow) >>= requireHandle "LoadCursorW"
  thread <- getCurrentThreadId
  inbox <- Inbox <$> newIORef (0, []) <*> newIORef Nothing <*> newIORef False <*> newIORef True <*> newIORef Nothing
  live <- newIORef True
  claimed <- newIORef False
  retained <- newIORef False
  callback <- makeWindowProc (dispatch inbox)
  let name = "HaskellGameIO." ++ show (ptrToWordPtr (castFunPtrToPtr callback))
  withWideString name $ \namePtr -> do
    register instanceHandle cursor namePtr callback
      `onException` freeHaskellFunPtr callback
    let unregister = unregisterAndFree namePtr instanceHandle callback
    hwnd <- (do
      dpi <- getDpiForSystem
      (width, height) <- adjustedSize dpi (windowWidth config) (windowHeight config)
      withWideString (windowTitle config) $ \titlePtr ->
        createWindowEx 0 namePtr titlePtr wsOverlappedWindow cwUseDefault cwUseDefault
          width height nullPtr nullPtr instanceHandle nullPtr
          >>= requireHandle "CreateWindowExW") `onException` unregister
    let window = Window hwnd instanceHandle name callback thread inbox live claimed retained
    (when visible (void (showWindow hwnd swShow)) >> checkCallback inbox >> pure window)
      `onException` closeWindow window

closeWindow :: Window -> IO ()
closeWindow window = mask_ $ do
  assertOwnerThread window
  writeIORef (windowLive window) False
  retained <- readIORef (windowRetained window)
  let hwnd = windowHandle window
  let instanceHandle = windowInstance window
  -- The class must be unregistered before GHC releases its callable address.
  -- If destruction or unregistration fails, keeping the callback alive is safe.
  let destroy = do
        alive <- isWindow hwnd
        when (alive /= 0) (requireBool "DestroyWindow" =<< destroyWindow hwnd)
  (do
    captured <- readIORef (inboxCapture (windowInbox window))
    when captured (releasePointer (windowInbox window) hwnd True))
    `finally` unless retained (destroy >> withWideString (windowClass window) (\name ->
      unregisterAndFree name instanceHandle (windowProc window)))

windowHandles :: Window -> (Handle, Handle)
windowHandles window = (windowInstance window, windowHandle window)

assertWindowThread :: Window -> IO ()
assertWindowThread window = do
  assertOwnerThread window
  live <- readIORef (windowLive window)
  unless live (throwIO WindowClosed)
  retained <- readIORef (windowRetained window)
  when retained (throwIO WindowNativeWorkUnconfirmed)

assertOwnerThread :: Window -> IO ()
assertOwnerThread window = do
  thread <- getCurrentThreadId
  unless (thread == windowThread window) (throwIO WrongWindowThread)

retainNativeWindow :: Window -> IO ()
retainNativeWindow window = writeIORef (windowRetained window) True

withWindowRenderer :: Window -> IO a -> IO a
withWindowRenderer window use = bracket acquire release (const use)
  where
    acquire = do
      assertWindowThread window
      claimed <- atomicModifyIORef' (windowClaimed window) (\current -> (True, current))
      when claimed (throwIO WindowRendererInUse)
    release () = writeIORef (windowClaimed window) False

windowFramebufferSize :: Window -> IO (Int, Int)
windowFramebufferSize window = do
  assertWindowThread window
  ifM (isIconic (windowHandle window))
    (pure (0, 0))
    (clientSize (windowHandle window))

resizeWindow :: Window -> Int -> Int -> IO ()
resizeWindow window width height = do
  assertWindowThread window
  checkExtent width height
  dpi <- getDpiForWindow (windowHandle window)
  (outerWidth, outerHeight) <- adjustedSize dpi width height
  requireBool "SetWindowPos" =<< setWindowPos (windowHandle window) nullPtr
    0 0 outerWidth outerHeight (swpNoMove .|. swpNoZOrder .|. swpNoActivate)

setWindowMinimumSize :: Window -> Int -> Int -> IO ()
setWindowMinimumSize window width height = do
  assertWindowThread window
  checkExtent width height
  writeIORef (inboxMinimumSize (windowInbox window)) (Just (width, height))

pollWindowEvents :: Window -> IO [WindowEvent]
pollWindowEvents window = map windowEvent <$> pollWindowEventsStamped window

pollWindowEventsStamped :: Window -> IO [TimedWindowEvent]
pollWindowEventsStamped window = do
  assertWindowThread window
  allocaBytesAligned msgSize 8 $ \message -> pump message
  let inbox = windowInbox window
  checkCallback inbox
  atomicModifyIORef' (inboxEvents inbox) (\(next, reversed) -> ((next, []), reverse reversed))
  where
    pump message = do
      pending <- peekMessage message nullPtr 0 0 pmRemove
      when (pending /= 0) $ do
        kind <- peekByteOff message msgMessageOffset
        if kind == wmQuit
          then enqueue (windowInbox window) CloseRequested
          else do
            void (translateMessage message)
            void (dispatchMessage message)
        pump message

-- | Confinement is available while this window is in the foreground. A
-- request racing with deactivation leaves capture off.
setPointerCapture :: Window -> Bool -> IO ()
setPointerCapture window requested = mask_ $ do
  assertWindowThread window
  let inbox = windowInbox window
  let hwnd = windowHandle window
  current <- readIORef (inboxCapture inbox)
  when (current /= requested) $
    if requested
      then acquirePointer inbox hwnd
      else releasePointer inbox hwnd True

windowPointerCaptured :: Window -> IO Bool
windowPointerCaptured window = do
  assertWindowThread window
  readIORef (inboxCapture (windowInbox window))

acquirePointer :: Inbox -> Handle -> IO ()
acquirePointer inbox hwnd = do
  foreground <- getForegroundWindow
  when (foreground == hwnd) $ do
    rawMouse hwnd 0
    let removeRaw = rawMouse nullPtr ridevRemove
    clipped <- clipToClient hwnd `onException` removeRaw
    if not clipped then removeRaw else do
      let rollback = releasePointer inbox hwnd True
      (captured, active) <- (do
          void (setCapture hwnd)
          (,) <$> getCapture <*> getForegroundWindow
        ) `onException` rollback
      if active == hwnd
        then (unless (captured == hwnd) (win32Failure "SetCapture") >> writeIORef (inboxCapture inbox) True)
          `onException` rollback
        else rollback

setPointerVisible :: Window -> Bool -> IO ()
setPointerVisible window visible = do
  assertWindowThread window
  let inbox = windowInbox window
  changed <- atomicModifyIORef' (inboxVisible inbox) (\previous -> (visible, previous /= visible))
  captured <- readIORef (inboxCapture inbox)
  -- WM_SETCURSOR maintains the policy during movement. Apply a change also
  -- when the pointer is stationary over our client area.
  applyNow <- if captured then pure True else if changed then pointerOverClient (windowHandle window) else pure False
  when applyNow $ do
    cursor <- if visible
      then loadCursor nullPtr (wordPtrToPtr idcArrow) >>= requireHandle "LoadCursorW"
      else pure nullPtr
    void (setCursor cursor)

pointerOverClient :: Handle -> IO Bool
pointerOverClient hwnd = allocaBytesAligned pointSize 8 $ \point -> do
  found <- getCursorPos point
  if found == 0 then pure False else do
    nativePoint <- peekByteOff point 0
    owner <- windowFromPoint nativePoint
    if owner /= hwnd then pure False else do
      mapped <- screenToClient hwnd point
      x <- fromIntegral <$> (peekByteOff point pointX :: IO Int32)
      y <- fromIntegral <$> (peekByteOff point pointY :: IO Int32)
      (width, height) <- clientSize hwnd
      pure (mapped /= 0 && x >= 0 && x < width && y >= 0 && y < height)

dispatch :: Inbox -> WindowProc
dispatch inbox hwnd message wParam lParam =
  mask $ \restore ->
    restore process `catch` \exception -> do
      writeIORef (inboxFailure inbox) (Just exception)
      defWindowProc hwnd message wParam lParam
  where
    process
      | message == wmClose = enqueue inbox CloseRequested >> pure 0
      | message == wmGetMinMaxInfo = do
          minimumSize <- readIORef (inboxMinimumSize inbox)
          case minimumSize of
            Nothing -> useDefault
            Just (width, height) -> do
              dpi <- getDpiForWindow hwnd
              (outerWidth, outerHeight) <- adjustedSize dpi width height
              let limits = intPtrToPtr lParam
              pokeByteOff limits minMaxMinimumWidth outerWidth
              pokeByteOff limits minMaxMinimumHeight outerHeight
              pure 0
      | message == wmSize = do
          (width, height) <- if wParam == sizeMinimized then pure (0, 0) else clientSize hwnd
          captured <- readIORef (inboxCapture inbox)
          when captured $ do
            clipped <- if width == 0 || height == 0 then pure False else clipToClient hwnd
            unless clipped $ do
              releasePointer inbox hwnd True
              enqueue inbox WindowPointerCaptureLost
          enqueue inbox (WindowResized width height)
          pure 0
      | message == wmDpiChanged = do
          let rect = intPtrToPtr lParam
          left <- peekByteOff rect rectLeft
          top <- peekByteOff rect rectTop
          right <- peekByteOff rect rectRight
          bottom <- peekByteOff rect rectBottom
          requireBool "SetWindowPos (WM_DPICHANGED)" =<< setWindowPos hwnd nullPtr
            left top (right - left) (bottom - top) (swpNoZOrder .|. swpNoActivate)
          pure 0
      | message == wmSetFocus = enqueue inbox (WindowFocusChanged True) >> pure 0
      | message == wmActivate && lowWord wParam == 0 = do
          captured <- readIORef (inboxCapture inbox)
          when captured $ do
            releasePointer inbox hwnd True
            enqueue inbox WindowPointerCaptureLost
          useDefault
      | message == wmKillFocus = do
          captured <- readIORef (inboxCapture inbox)
          when captured (releasePointer inbox hwnd True)
          enqueue inbox (WindowFocusChanged False)
          pure 0
      | message == wmCaptureChanged = do
          captured <- readIORef (inboxCapture inbox)
          when captured $ do
            releasePointer inbox hwnd False
            enqueue inbox WindowPointerCaptureLost
          pure 0
      | message == wmSetCursor && lowWord lParam == htClient = do
          visible <- readIORef (inboxVisible inbox)
          if visible then useDefault else setCursor nullPtr >> pure 1
      | message == wmInput = rawMotion inbox lParam >> useDefault
      | message == wmKeyDown || message == wmKeyUp = keyEvent >> pure 0
      | message == wmSysKeyDown || message == wmSysKeyUp = keyEvent >> useDefault
      | message == wmChar = enqueue inbox (WindowTextCodeUnit (fromIntegral wParam)) >> pure 0
      | message == wmMouseMove = enqueue inbox (WindowPointerMoved x y) >> pure 0
      | Just (button, down) <- mouseButton message wParam = do
          enqueue inbox (WindowPointerButton button down x y)
          pure (if button == PointerX1 || button == PointerX2 then 1 else 0)
      | message == wmMouseWheel || message == wmMouseHWheel = do
          (clientX, clientY) <- screenPoint hwnd x y
          let delta = signedHighWord wParam
          let (horizontal, vertical) = if message == wmMouseWheel then (0, delta) else (delta, 0)
          enqueue inbox (WindowPointerWheel horizontal vertical clientX clientY)
          pure 0
      | otherwise = useDefault
    useDefault = defWindowProc hwnd message wParam lParam
    x = fromIntegral (fromIntegral lParam :: Int16)
    y = signedHighWord lParam
    keyEvent = enqueue inbox $ WindowKeyChanged KeyMessage
      { keyVirtual = fromIntegral wParam
      , keyScanCode = fromIntegral ((lParam `shiftR` 16) .&. 0xff)
      , keyExtended = testBit lParam 24
      , keyDown = message == wmKeyDown || message == wmSysKeyDown
      , keyRepeated = (message == wmKeyDown || message == wmSysKeyDown) && testBit lParam 30
      }

mouseButton :: Word32 -> WordPtr -> Maybe (PointerButton, Bool)
mouseButton message wParam
  | message == wmLeftDown = Just (PointerLeft, True)
  | message == wmLeftUp = Just (PointerLeft, False)
  | message == wmRightDown = Just (PointerRight, True)
  | message == wmRightUp = Just (PointerRight, False)
  | message == wmMiddleDown = Just (PointerMiddle, True)
  | message == wmMiddleUp = Just (PointerMiddle, False)
  | message == wmXDown = Just (xButton, True)
  | message == wmXUp = Just (xButton, False)
  | otherwise = Nothing
  where
    xButton = if signedHighWord wParam == 1 then PointerX1 else PointerX2

enqueue :: Inbox -> WindowEvent -> IO ()
enqueue inbox event = do
  time <- getMonotonicTimeNSec
  atomicModifyIORef' (inboxEvents inbox) $ \(next, events) ->
    let timed = TimedWindowEvent time next event
        following = next + 1
    in following `seq` ((following, timed : events), ())

checkCallback :: Inbox -> IO ()
checkCallback inbox = do
  failure <- atomicModifyIORef' (inboxFailure inbox) (\saved -> (Nothing, saved))
  maybe (pure ()) throwIO failure

register :: Handle -> Handle -> Ptr Word16 -> FunPtr WindowProc -> IO ()
register instanceHandle cursor name callback = allocaBytesAligned windowClassSize 8 $ \cls -> do
  fillBytes cls 0 windowClassSize
  pokeByteOff cls wcSize (fromIntegral windowClassSize :: Word32)
  pokeByteOff cls wcProc callback
  pokeByteOff cls wcInstance instanceHandle
  pokeByteOff cls wcCursor cursor
  pokeByteOff cls wcName name
  atom <- registerClassEx cls
  when (atom == 0) (win32Failure "RegisterClassExW")

unregisterAndFree :: Ptr Word16 -> Handle -> FunPtr WindowProc -> IO ()
unregisterAndFree name instanceHandle callback = do
  requireBool "UnregisterClassW" =<< unregisterClass name instanceHandle
  freeHaskellFunPtr callback

checkExtent :: Int -> Int -> IO ()
checkExtent width height =
  when (width <= 0 || height <= 0 || width > largest || height > largest) $
    throwIO (InvalidWindowSize width height)
  where
    largest = fromIntegral (maxBound :: Int32) - 4096

clientSize :: Handle -> IO (Int, Int)
clientSize hwnd = allocaBytesAligned rectSize 4 $ \rect -> do
  requireBool "GetClientRect" =<< getClientRect hwnd rect
  right <- peekByteOff rect rectRight :: IO Int32
  bottom <- peekByteOff rect rectBottom :: IO Int32
  pure (fromIntegral right, fromIntegral bottom)

adjustedSize :: Word32 -> Int -> Int -> IO (Int32, Int32)
adjustedSize dpi width height = allocaBytesAligned rectSize 4 $ \rect -> do
  fillBytes rect 0 rectSize
  pokeByteOff rect rectRight (fromIntegral width :: Int32)
  pokeByteOff rect rectBottom (fromIntegral height :: Int32)
  requireBool "AdjustWindowRectExForDpi" =<< adjustWindowRectExForDpi
    rect wsOverlappedWindow 0 0 dpi
  left <- peekByteOff rect rectLeft
  top <- peekByteOff rect rectTop
  right <- peekByteOff rect rectRight
  bottom <- peekByteOff rect rectBottom
  pure (right - left, bottom - top)

screenPoint :: Handle -> Int -> Int -> IO (Int, Int)
screenPoint hwnd x y = allocaBytesAligned pointSize 4 $ \point -> do
  pokeByteOff point pointX (fromIntegral x :: Int32)
  pokeByteOff point pointY (fromIntegral y :: Int32)
  requireBool "ScreenToClient" =<< screenToClient hwnd point
  clientX <- peekByteOff point pointX :: IO Int32
  clientY <- peekByteOff point pointY :: IO Int32
  pure (fromIntegral clientX, fromIntegral clientY)

clipToClient :: Handle -> IO Bool
clipToClient hwnd = do
  foreground <- getForegroundWindow
  if foreground /= hwnd then pure False else
    allocaBytesAligned rectSize 4 $ \rect -> do
      requireBool "GetClientRect" =<< getClientRect hwnd rect
      requireBool "ClientToScreen (top left)" =<< clientToScreen hwnd rect
      requireBool "ClientToScreen (bottom right)" =<< clientToScreen hwnd (rect `plusPtr` rectRight)
      setClip hwnd "ClipCursor" rect

releasePointer :: Inbox -> Handle -> Bool -> IO ()
releasePointer inbox hwnd releaseOsCapture = do
  writeIORef (inboxCapture inbox) False
  let unclip = void (setClip hwnd "ClipCursor (release)" nullPtr)
  let release = when releaseOsCapture $ do
        captured <- getCapture
        when (captured == hwnd) (requireBool "ReleaseCapture" =<< releaseCapture)
  unclip `finally` (release `finally` rawMouse nullPtr ridevRemove)

setClip :: Handle -> String -> Ptr () -> IO Bool
setClip hwnd operation rect = do
  foreground <- getForegroundWindow
  -- Another foreground window may own the shared clipping rectangle now.
  if foreground /= hwnd && (rect /= nullPtr || foreground /= nullPtr)
    then pure False
    else do
      success <- clipCursor rect
      if success /= 0 then pure True else do
        errorCode <- getLastError
        active <- getForegroundWindow
        if errorCode == 5 && active /= hwnd
          then pure False
          else throwIO (Win32Failed operation errorCode)

rawMouse :: Handle -> Word32 -> IO ()
rawMouse hwnd flags = allocaBytesAligned rawDeviceSize 8 $ \device -> do
  pokeByteOff device 0 (1 :: Word16)
  pokeByteOff device 2 (2 :: Word16)
  pokeByteOff device 4 flags
  pokeByteOff device 8 hwnd
  requireBool "RegisterRawInputDevices" =<< registerRawInputDevices device 1 (fromIntegral rawDeviceSize)

rawMotion :: Inbox -> IntPtr -> IO ()
rawMotion inbox lParam = do
  captured <- readIORef (inboxCapture inbox)
  when captured $ alloca $ \size -> allocaBytesAligned rawMouseSize 8 $ \raw -> do
    poke size (fromIntegral rawMouseSize)
    count <- getRawInputData (intPtrToPtr lParam) ridInput raw size (fromIntegral rawHeaderSize)
    when (count == fromIntegral rawMouseSize) $ do
      kind <- peekByteOff raw rawType :: IO Word32
      flags <- peekByteOff raw rawMouseFlags :: IO Word16
      when (kind == rimTypeMouse && not (testBit flags 0)) $ do
        dx <- peekByteOff raw rawMouseX :: IO Int32
        dy <- peekByteOff raw rawMouseY :: IO Int32
        when (dx /= 0 || dy /= 0) $ enqueue inbox (WindowPointerDelta (fromIntegral dx) (fromIntegral dy))

withWideString :: String -> (Ptr Word16 -> IO a) -> IO a
withWideString value = withArray0 0 (concatMap utf16 value)
  where
    utf16 char
      | code <= 0xffff = [fromIntegral code]
      | otherwise =
          let supplementary = code - 0x10000
          in [fromIntegral (0xd800 + (supplementary `shiftR` 10)),
              fromIntegral (0xdc00 + (supplementary .&. 0x3ff))]
      where
        code = ord char

signedHighWord :: (Integral a, Bits a) => a -> Int
signedHighWord value = fromIntegral (fromIntegral (value `shiftR` 16) :: Int16)

lowWord :: Integral a => a -> Word16
lowWord = fromIntegral

ifM :: IO WinBool -> IO a -> IO a -> IO a
ifM condition yes no = condition >>= \value -> if value /= 0 then yes else no

requireHandle :: String -> Handle -> IO Handle
requireHandle operation handle
  | handle == nullPtr = win32Failure operation
  | otherwise = pure handle

requireBool :: String -> WinBool -> IO ()
requireBool operation ok = when (ok == 0) (win32Failure operation)

win32Failure :: String -> IO a
win32Failure operation = getLastError >>= throwIO . Win32Failed operation

-- Layouts for the Windows SDK x64 ABI. All pointers in these records are 8 bytes.
windowClassSize, wcSize, wcProc, wcInstance, wcCursor, wcName :: Int
windowClassSize = 80
wcSize = 0
wcProc = 8
wcInstance = 24
wcCursor = 40
wcName = 64

msgSize, msgMessageOffset, rectSize, rectLeft, rectTop, rectRight, rectBottom :: Int
msgSize = 48
msgMessageOffset = 8
rectSize = 16
rectLeft = 0
rectTop = 4
rectRight = 8
rectBottom = 12

pointSize, pointX, pointY, rawDeviceSize, rawHeaderSize, rawMouseSize :: Int
pointSize = 8
pointX = 0
pointY = 4
rawDeviceSize = 16
rawHeaderSize = 24
rawMouseSize = 48

rawType, rawMouseFlags, rawMouseX, rawMouseY :: Int
rawType = 0
rawMouseFlags = 24
rawMouseX = 36
rawMouseY = 40

minMaxMinimumWidth, minMaxMinimumHeight :: Int
minMaxMinimumWidth = 24
minMaxMinimumHeight = 28

perMonitorAwareV2 :: Handle
perMonitorAwareV2 = intPtrToPtr (-4)

wsOverlappedWindow, pmRemove, swpNoMove, swpNoZOrder, swpNoActivate :: Word32
wsOverlappedWindow = 0x00cf0000
pmRemove = 1
swpNoMove = 0x0002
swpNoZOrder = 0x0004
swpNoActivate = 0x0010

cwUseDefault, swShow :: Int32
cwUseDefault = minBound
swShow = 5

idcArrow, sizeMinimized :: WordPtr
idcArrow = 32512
sizeMinimized = 1

htClient :: Word16
htClient = 1

ridInput, ridevRemove, rimTypeMouse :: Word32
ridInput = 0x10000003
ridevRemove = 1
rimTypeMouse = 0

wmSize, wmActivate, wmSetFocus, wmKillFocus, wmClose, wmQuit, wmSetCursor, wmGetMinMaxInfo, wmInput :: Word32
wmSize = 0x0005
wmActivate = 0x0006
wmSetFocus = 0x0007
wmKillFocus = 0x0008
wmClose = 0x0010
wmQuit = 0x0012
wmSetCursor = 0x0020
wmGetMinMaxInfo = 0x0024
wmInput = 0x00ff

wmKeyDown, wmKeyUp, wmChar, wmSysKeyDown, wmSysKeyUp, wmDpiChanged :: Word32
wmKeyDown = 0x0100
wmKeyUp = 0x0101
wmChar = 0x0102
wmSysKeyDown = 0x0104
wmSysKeyUp = 0x0105
wmDpiChanged = 0x02e0

wmMouseMove, wmLeftDown, wmLeftUp, wmRightDown, wmRightUp, wmMiddleDown, wmMiddleUp :: Word32
wmMouseMove = 0x0200
wmLeftDown = 0x0201
wmLeftUp = 0x0202
wmRightDown = 0x0204
wmRightUp = 0x0205
wmMiddleDown = 0x0207
wmMiddleUp = 0x0208

wmMouseWheel, wmMouseHWheel, wmXDown, wmXUp, wmCaptureChanged :: Word32
wmMouseWheel = 0x020a
wmMouseHWheel = 0x020e
wmXDown = 0x020b
wmXUp = 0x020c
wmCaptureChanged = 0x0215

foreign import ccall "wrapper" makeWindowProc :: WindowProc -> IO (FunPtr WindowProc)
foreign import ccall unsafe "GetCurrentThreadId" getCurrentThreadId :: IO Word32
foreign import ccall unsafe "GetLastError" getLastError :: IO Word32
foreign import ccall unsafe "GetModuleHandleW" getModuleHandle :: Ptr Word16 -> IO Handle
foreign import ccall safe "LoadCursorW" loadCursor :: Handle -> Ptr Word16 -> IO Handle
foreign import ccall safe "RegisterClassExW" registerClassEx :: Ptr () -> IO Word16
foreign import ccall safe "UnregisterClassW" unregisterClass :: Ptr Word16 -> Handle -> IO WinBool
foreign import ccall safe "CreateWindowExW" createWindowEx
  :: Word32 -> Ptr Word16 -> Ptr Word16 -> Word32 -> Int32 -> Int32 -> Int32 -> Int32
  -> Handle -> Handle -> Handle -> Ptr () -> IO Handle
foreign import ccall safe "DestroyWindow" destroyWindow :: Handle -> IO WinBool
foreign import ccall unsafe "IsWindow" isWindow :: Handle -> IO WinBool
foreign import ccall unsafe "IsIconic" isIconic :: Handle -> IO WinBool
foreign import ccall safe "DefWindowProcW" defWindowProc :: WindowProc
foreign import ccall safe "ShowWindow" showWindow :: Handle -> Int32 -> IO WinBool
foreign import ccall safe "PeekMessageW" peekMessage
  :: Ptr () -> Handle -> Word32 -> Word32 -> Word32 -> IO WinBool
foreign import ccall safe "TranslateMessage" translateMessage :: Ptr () -> IO WinBool
foreign import ccall safe "DispatchMessageW" dispatchMessage :: Ptr () -> IO LResult
foreign import ccall unsafe "GetClientRect" getClientRect :: Handle -> Ptr () -> IO WinBool
foreign import ccall safe "SetWindowPos" setWindowPos
  :: Handle -> Handle -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO WinBool
foreign import ccall unsafe "AdjustWindowRectExForDpi" adjustWindowRectExForDpi
  :: Ptr () -> Word32 -> WinBool -> Word32 -> Word32 -> IO WinBool
foreign import ccall unsafe "GetDpiForWindow" getDpiForWindow :: Handle -> IO Word32
foreign import ccall unsafe "GetDpiForSystem" getDpiForSystem :: IO Word32
foreign import ccall unsafe "SetThreadDpiAwarenessContext" setThreadDpiAwarenessContext :: Handle -> IO Handle
foreign import ccall unsafe "ScreenToClient" screenToClient :: Handle -> Ptr () -> IO WinBool
foreign import ccall unsafe "ClientToScreen" clientToScreen :: Handle -> Ptr () -> IO WinBool
foreign import ccall safe "ClipCursor" clipCursor :: Ptr () -> IO WinBool
foreign import ccall safe "SetCapture" setCapture :: Handle -> IO Handle
foreign import ccall safe "ReleaseCapture" releaseCapture :: IO WinBool
foreign import ccall unsafe "GetCapture" getCapture :: IO Handle
foreign import ccall unsafe "GetForegroundWindow" getForegroundWindow :: IO Handle
foreign import ccall unsafe "GetCursorPos" getCursorPos :: Ptr () -> IO WinBool
-- The Windows x64 ABI passes the eight-byte POINT aggregate by value.
foreign import ccall safe "WindowFromPoint" windowFromPoint :: Word64 -> IO Handle
foreign import ccall unsafe "SetCursor" setCursor :: Handle -> IO Handle
foreign import ccall safe "RegisterRawInputDevices" registerRawInputDevices
  :: Ptr () -> Word32 -> Word32 -> IO WinBool
foreign import ccall unsafe "GetRawInputData" getRawInputData
  :: Handle -> Word32 -> Ptr () -> Ptr Word32 -> Word32 -> IO Word32
