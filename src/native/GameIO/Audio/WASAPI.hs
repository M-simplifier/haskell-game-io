module GameIO.Audio.WASAPI
  ( Event
  , newEvent
  , closeEvent
  , signalEvent
  , Wake (..)
  , withControlWait
  , withDeviceWait
  , RenderDevice
  , withRenderDevice
  , deviceFormat
  , devicePadding
  , withStartedDevice
  , withOutputPacket
  , recoverableDeviceError
  , withLoopbackProbe
  ) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket, bracket_, mask, onException, throwIO)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.String (CWString, withCWString)
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, peekByteOff, peekElemOff, poke, pokeByteOff)
import GameIO.Audio.Types

-- The three COM interfaces are only touched by the owning bound OS thread.
data Enumerator
data Endpoint
data AudioClient
data RenderClient
data CaptureClient

newtype Event = Event (Ptr ())

data Wake = Shutdown | Refresh | DeviceReady | Timeout
  deriving (Eq, Show)

type HResult = Int32
type GuidPtr = Ptr ()

data Guid = Guid !Word32 !Word16 !Word16
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8

clsidEnumerator, iidEnumerator, iidAudioClient, iidRenderClient, iidCaptureClient :: Guid
clsidEnumerator = Guid 0xbcde0395 0xe52f 0x467c 0x8e 0x3d 0xc4 0x57 0x92 0x91 0x69 0x2e
iidEnumerator = Guid 0xa95664d2 0x9614 0x4f35 0xa7 0x46 0xde 0x8d 0xb6 0x36 0x17 0xe6
iidAudioClient = Guid 0x1cb9ad4c 0xdbfa 0x4c32 0xb1 0x78 0xc2 0xf5 0x68 0xa7 0x03 0xb2
iidRenderClient = Guid 0xf294acfc 0x3146 0x4483 0xa7 0xbf 0xad 0xdc 0xa7 0xc2 0x60 0xe2
iidCaptureClient = Guid 0xc8adbd64 0xe71e 0x48a0 0xa4 0xde 0x18 0x5c 0x39 0x5c 0xd3 0x17

withGuid :: Guid -> (GuidPtr -> IO a) -> IO a
withGuid (Guid a b c d e f g h i j k) action = allocaBytesAligned 16 4 $ \pointer -> do
  pokeByteOff pointer 0 a
  pokeByteOff pointer 4 b
  pokeByteOff pointer 6 c
  pokeByteOff pointer 8 d
  pokeByteOff pointer 9 e
  pokeByteOff pointer 10 f
  pokeByteOff pointer 11 g
  pokeByteOff pointer 12 h
  pokeByteOff pointer 13 i
  pokeByteOff pointer 14 j
  pokeByteOff pointer 15 k
  action pointer

-- Windows x64 has a single calling convention, including COM vtable methods.
foreign import ccall safe "CoInitializeEx" coInitializeEx :: Ptr () -> Word32 -> IO HResult
foreign import ccall safe "CoUninitialize" coUninitialize :: IO ()
foreign import ccall safe "CoCreateInstance"
  coCreateInstance :: GuidPtr -> Ptr () -> Word32 -> GuidPtr -> Ptr (Ptr Enumerator) -> IO HResult
foreign import ccall unsafe "CoTaskMemFree" coTaskMemFree :: Ptr () -> IO ()
foreign import ccall unsafe "CreateEventW"
  createEvent :: Ptr () -> Int32 -> Int32 -> CWString -> IO (Ptr ())
foreign import ccall unsafe "SetEvent" setEvent :: Ptr () -> IO Int32
foreign import ccall unsafe "CloseHandle" closeHandle :: Ptr () -> IO Int32
foreign import ccall unsafe "GetLastError" getLastError :: IO Word32
foreign import ccall safe "WaitForMultipleObjects"
  waitForMultipleObjects :: Word32 -> Ptr (Ptr ()) -> Int32 -> Word32 -> IO Word32
foreign import ccall safe "AvSetMmThreadCharacteristicsW"
  avSetCharacteristics :: CWString -> Ptr Word32 -> IO (Ptr ())
foreign import ccall safe "AvSetMmThreadPriority"
  avSetPriority :: Ptr () -> Int32 -> IO Int32
foreign import ccall safe "AvRevertMmThreadCharacteristics"
  avRevertCharacteristics :: Ptr () -> IO Int32

type Release = Ptr () -> IO Word32
type GetDefaultEndpoint = Ptr Enumerator -> Int32 -> Int32 -> Ptr (Ptr Endpoint) -> IO HResult
type Activate = Ptr Endpoint -> GuidPtr -> Word32 -> Ptr () -> Ptr (Ptr AudioClient) -> IO HResult
type Initialize = Ptr AudioClient -> Int32 -> Word32 -> Int64 -> Int64 -> Ptr () -> GuidPtr -> IO HResult
type ClientFrames = Ptr AudioClient -> Ptr Word32 -> IO HResult
type ClientAction = Ptr AudioClient -> IO HResult
type GetMixFormat = Ptr AudioClient -> Ptr (Ptr ()) -> IO HResult
type GetDevicePeriod = Ptr AudioClient -> Ptr Int64 -> Ptr Int64 -> IO HResult
type SetEventHandle = Ptr AudioClient -> Ptr () -> IO HResult
type GetService = Ptr AudioClient -> GuidPtr -> Ptr (Ptr ()) -> IO HResult
type GetBuffer = Ptr RenderClient -> Word32 -> Ptr (Ptr Float) -> IO HResult
type ReleaseBuffer = Ptr RenderClient -> Word32 -> Word32 -> IO HResult
type GetCaptureBuffer =
  Ptr CaptureClient -> Ptr (Ptr Float) -> Ptr Word32 -> Ptr Word32 -> Ptr Word64 -> Ptr Word64 -> IO HResult
type ReleaseCaptureBuffer = Ptr CaptureClient -> Word32 -> IO HResult
type CapturePacketSize = Ptr CaptureClient -> Ptr Word32 -> IO HResult

foreign import ccall safe "dynamic" callRelease :: FunPtr Release -> Release
foreign import ccall safe "dynamic" callGetDefaultEndpoint :: FunPtr GetDefaultEndpoint -> GetDefaultEndpoint
foreign import ccall safe "dynamic" callActivate :: FunPtr Activate -> Activate
foreign import ccall safe "dynamic" callInitialize :: FunPtr Initialize -> Initialize
foreign import ccall safe "dynamic" callGetBufferSize :: FunPtr ClientFrames -> ClientFrames
foreign import ccall unsafe "dynamic" callGetPadding :: FunPtr ClientFrames -> ClientFrames
foreign import ccall safe "dynamic" callClientAction :: FunPtr ClientAction -> ClientAction
foreign import ccall safe "dynamic" callGetMixFormat :: FunPtr GetMixFormat -> GetMixFormat
foreign import ccall safe "dynamic" callGetDevicePeriod :: FunPtr GetDevicePeriod -> GetDevicePeriod
foreign import ccall safe "dynamic" callSetEventHandle :: FunPtr SetEventHandle -> SetEventHandle
foreign import ccall safe "dynamic" callGetService :: FunPtr GetService -> GetService
foreign import ccall unsafe "dynamic" callGetBuffer :: FunPtr GetBuffer -> GetBuffer
foreign import ccall unsafe "dynamic" callReleaseBuffer :: FunPtr ReleaseBuffer -> ReleaseBuffer
foreign import ccall unsafe "dynamic" callGetCaptureBuffer :: FunPtr GetCaptureBuffer -> GetCaptureBuffer
foreign import ccall unsafe "dynamic" callReleaseCaptureBuffer :: FunPtr ReleaseCaptureBuffer -> ReleaseCaptureBuffer
foreign import ccall unsafe "dynamic" callCapturePacketSize :: FunPtr CapturePacketSize -> CapturePacketSize

method :: Ptr object -> Int -> IO (FunPtr f)
method object slot = do
  table <- peek (castPtr object)
  peekElemOff table slot

checkResult :: String -> HResult -> IO ()
checkResult operation result = when (result < 0) (throwIO (WasapiError operation result))
{-# INLINE checkResult #-}

throwWin32 :: String -> IO a
throwWin32 operation = getLastError >>= throwIO . AudioWin32Error operation

withInterface :: String -> (Ptr (Ptr object) -> IO HResult) -> (Ptr object -> IO a) -> IO a
withInterface operation acquire = bracket acquireInterface releaseInterface
  where
    acquireInterface = alloca $ \output -> do
      checkResult operation =<< acquire output
      peek output
    releaseInterface object = do
      release <- callRelease <$> method object 2 -- IUnknown::Release
      void (release (castPtr object))

newEvent :: IO Event
newEvent = runInBoundThread $ do
  handle <- createEvent nullPtr 0 0 nullPtr
  when (handle == nullPtr) (throwWin32 "CreateEventW")
  pure (Event handle)

closeEvent :: Event -> IO ()
closeEvent (Event handle) = void (closeHandle handle)

signalEvent :: Event -> IO ()
signalEvent (Event handle) = runInBoundThread $ do
  result <- setEvent handle
  when (result == 0) (throwWin32 "SetEvent")

withControlWait :: Event -> Event -> (IO Wake -> IO a) -> IO a
withControlWait (Event shutdown) (Event refresh) action =
  withArray [shutdown, refresh] $ \handles ->
    action (waitEvents 2 handles 250)

withDeviceWait :: Event -> Event -> RenderDevice -> (IO Wake -> IO a) -> IO a
withDeviceWait (Event shutdown) (Event refresh) device action =
  let Event ready = deviceEvent device
  in withArray [shutdown, refresh, ready] $ \handles ->
    action (waitEvents 3 handles 250)

waitEvents :: Word32 -> Ptr (Ptr ()) -> Word32 -> IO Wake
waitEvents count handles milliseconds = do
  result <- waitForMultipleObjects count handles 0 milliseconds
  case result of
    0 -> pure Shutdown
    1 -> pure Refresh
    2 -> pure DeviceReady
    258 -> pure Timeout
    _ -> throwWin32 "WaitForMultipleObjects"

withCom :: IO a -> IO a
withCom = bracket_
  (coInitializeEx nullPtr 0 >>= checkResult "CoInitializeEx(MTA)")
  coUninitialize

clsctxAll :: Word32
clsctxAll = 23

withDefaultClient :: (Ptr AudioClient -> IO a) -> IO a
withDefaultClient action =
  withGuid clsidEnumerator $ \classId ->
  withGuid iidEnumerator $ \enumeratorId ->
  withInterface "CoCreateInstance(MMDeviceEnumerator)"
    (coCreateInstance classId nullPtr clsctxAll enumeratorId) $ \enumerator -> do
      getDefaultEndpoint <- callGetDefaultEndpoint <$> method enumerator 4
      withInterface "IMMDeviceEnumerator::GetDefaultAudioEndpoint"
        (getDefaultEndpoint enumerator 0 0) $ \endpoint -> do -- eRender/eConsole
          activate <- callActivate <$> method endpoint 3
          withGuid iidAudioClient $ \clientId ->
            withInterface "IMMDevice::Activate(IAudioClient)"
              (activate endpoint clientId clsctxAll nullPtr) action

withMmcss :: (Bool -> IO a) -> IO a
withMmcss action = withCWString "Pro Audio" $ \taskName -> alloca $ \taskIndex ->
  bracket (poke taskIndex 0 >> avSetCharacteristics taskName taskIndex) release $ \registration ->
    if registration == nullPtr
      then action False
      else do
        void (avSetPriority registration 1) -- AVRT_PRIORITY_HIGH
        action True
  where
    release registration = unless (registration == nullPtr) (void (avRevertCharacteristics registration))

withFloatFormat :: Int -> (Ptr () -> IO a) -> IO a
withFloatFormat rate action = allocaBytesAligned 18 4 $ \format -> do
  -- WAVEFORMATEX is packed. Offsets match mmreg.h.
  pokeByteOff format 0 (3 :: Word16) -- WAVE_FORMAT_IEEE_FLOAT
  pokeByteOff format 2 (2 :: Word16) -- Stereo
  pokeByteOff format 4 (fromIntegral rate :: Word32)
  pokeByteOff format 8 (fromIntegral (rate * 8) :: Word32)
  pokeByteOff format 12 (8 :: Word16) -- Bytes per stereo frame
  pokeByteOff format 14 (32 :: Word16)
  pokeByteOff format 16 (0 :: Word16) -- No extra format bytes
  action format

clientMixRate :: Ptr AudioClient -> IO Int
clientMixRate client = do
  getMixFormat <- callGetMixFormat <$> method client 8
  bracket
    (alloca $ \output -> getMixFormat client output >>= checkResult "IAudioClient::GetMixFormat" >> peek output)
    coTaskMemFree
    (\format -> fromIntegral <$> (peekByteOff format 4 :: IO Word32))

clientDevicePeriod :: Ptr AudioClient -> IO Int64
clientDevicePeriod client = alloca $ \period -> do
  getPeriod <- callGetDevicePeriod <$> method client 9
  checkResult "IAudioClient::GetDevicePeriod" =<< getPeriod client period nullPtr
  peek period

initializeEventClient :: Ptr AudioClient -> Event -> Int -> Int -> Word32 -> IO ()
initializeEventClient client (Event handle) rate bufferMs extraFlags = do
  initialize <- callInitialize <$> method client 3
  withFloatFormat rate $ \format -> do
    let eventCallback = 0x00040000
        noPersist = 0x00080000
        srcDefaultQuality = 0x08000000
        autoConvertPCM = 0x80000000
        flags = eventCallback .|. noPersist .|. srcDefaultQuality .|. autoConvertPCM .|. extraFlags
    checkResult "IAudioClient::Initialize(shared/event Float32 stereo)"
      =<< initialize client 0 flags (fromIntegral bufferMs * 10000) 0 format nullPtr
  setEventHandle <- callSetEventHandle <$> method client 13
  checkResult "IAudioClient::SetEventHandle" =<< setEventHandle client handle

withClientService :: String -> Guid -> Ptr AudioClient -> (Ptr object -> IO a) -> IO a
withClientService name identifier client action = do
  getService <- callGetService <$> method client 14
  withGuid identifier $ \serviceId ->
    withInterface name (getService client serviceId . castPtr) action

data RenderDevice = RenderDevice
  { deviceFormat :: !AudioFormat
  , deviceEvent :: !Event
  , startDevice :: !(IO HResult)
  , stopDevice :: !(IO HResult)
  , getPadding :: !(Ptr Word32 -> IO HResult)
  , paddingPointer :: !(Ptr Word32)
  , getPacket :: !(Word32 -> Ptr (Ptr Float) -> IO HResult)
  , releasePacket :: !(Word32 -> Word32 -> IO HResult)
  , packetPointer :: !(Ptr (Ptr Float))
  }

withRenderDevice :: AudioConfig -> (RenderDevice -> IO a) -> IO a
withRenderDevice (AudioConfig rate bufferMs _) action = withCom $
  bracket newEvent closeEvent $ \ready ->
    withDefaultClient $ \client -> do
      endpointRate <- clientMixRate client
      period <- clientDevicePeriod client
      initializeEventClient client ready rate bufferMs 0
      frames <- alloca $ \output -> do
        getBufferSize <- callGetBufferSize <$> method client 4
        checkResult "IAudioClient::GetBufferSize" =<< getBufferSize client output
        fromIntegral <$> peek output
      withClientService "IAudioClient::GetService(IAudioRenderClient)" iidRenderClient client $ \render ->
        withMmcss $ \mmcss -> alloca $ \padding -> alloca $ \packet -> do
          start <- flip callClientAction client <$> method client 10
          stop <- flip callClientAction client <$> method client 11
          queryPadding <- flip callGetPadding client <$> method client 6
          acquire <- flip callGetBuffer render <$> method render 3
          release <- flip callReleaseBuffer render <$> method render 4
          let format = AudioFormat rate Stereo Float32LE frames (fromIntegral period * 100) endpointRate mmcss
          action (RenderDevice format ready start stop queryPadding padding acquire release packet)

devicePadding :: RenderDevice -> IO Int
devicePadding device = do
  let output = paddingPointer device
  checkResult "IAudioClient::GetCurrentPadding" =<< getPadding device output
  fromIntegral <$> peek output

withStartedDevice :: RenderDevice -> IO a -> IO a
withStartedDevice device = bracket_ start (void (stopDevice device))
  where
    start = do
      let frames = fromIntegral (audioDeviceBufferFrames (deviceFormat device))
      checkResult "IAudioRenderClient::GetBuffer(prime)" =<< getPacket device frames (packetPointer device)
      checkResult "IAudioRenderClient::ReleaseBuffer(prime)" =<< releasePacket device frames silentPacket
      checkResult "IAudioClient::Start" =<< startDevice device

silentPacket :: Word32
silentPacket = 0x2

withOutputPacket :: RenderDevice -> Int -> (Ptr Float -> IO a) -> IO a
withOutputPacket device frameCount action = mask $ \restore -> do
  let frames = fromIntegral frameCount
      output = packetPointer device
      submit = releasePacket device frames
  checkResult "IAudioRenderClient::GetBuffer" =<< getPacket device frames output
  result <- (peek output >>= restore . action) `onException` void (submit silentPacket)
  checkResult "IAudioRenderClient::ReleaseBuffer" =<< submit 0
  pure result

recoverableDeviceError :: AudioError -> Bool
recoverableDeviceError (WasapiError _ result) = case (fromIntegral result :: Word32) of
  0x88890004 -> True -- AUDCLNT_E_DEVICE_INVALIDATED
  0x88890010 -> True -- AUDCLNT_E_SERVICE_NOT_RUNNING
  0x88890026 -> True -- AUDCLNT_E_RESOURCES_INVALIDATED
  0x80070490 -> True -- The default endpoint is currently absent.
  _ -> False
recoverableDeviceError _ = False

-- Internal endpoint observation used by the Windows audio validation tool.
-- Captures the engine mix and never opens a microphone.
type ReadLoopback = (Ptr Float -> Int -> Word32 -> IO ()) -> IO Bool

withLoopbackProbe :: Int -> (IO Bool -> ReadLoopback -> IO a) -> IO a
withLoopbackProbe rate action = runInBoundThread $ withCom $
  bracket newEvent closeEvent $ \ready@(Event handle) ->
    withDefaultClient $ \client -> do
      let loopbackFlag = 0x00020000
      initializeEventClient client ready rate 100 loopbackFlag
      withClientService "IAudioClient::GetService(IAudioCaptureClient)" iidCaptureClient client $ \capture -> do
        start <- flip callClientAction client <$> method client 10
        stop <- flip callClientAction client <$> method client 11
        packetSize <- flip callCapturePacketSize capture <$> method capture 5
        readPacket <- flip callGetCaptureBuffer capture <$> method capture 3
        release <- flip callReleaseCaptureBuffer capture <$> method capture 4
        withArray [handle] $ \handles ->
          alloca $ \buffer -> alloca $ \frames -> alloca $ \flags ->
            let wait = (== Shutdown) <$> waitEvents 1 handles 500
                readOne consumer = mask $ \restore -> do
                  checkResult "IAudioCaptureClient::GetNextPacketSize" =<< packetSize frames
                  available <- peek frames
                  if available == 0 then pure False else do
                    checkResult "IAudioCaptureClient::GetBuffer"
                      =<< readPacket buffer frames flags nullPtr nullPtr
                    captured <- peek frames
                    let submit count = checkResult "IAudioCaptureClient::ReleaseBuffer" =<< release count
                    _ <- (do
                      pointer <- peek buffer
                      packetFlags <- peek flags
                      restore (consumer pointer (fromIntegral captured) packetFlags))
                      `onException` void (release captured)
                    submit captured
                    pure True
            in bracket_ (start >>= checkResult "IAudioClient::Start(loopback)") (void stop)
              (action wait readOne)
