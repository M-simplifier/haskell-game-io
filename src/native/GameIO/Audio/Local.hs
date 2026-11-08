-- | Shared-mode native audio for a Windows x64 game. Build the executable
-- with the threaded RTS. Audio time is measured in stereo output frames.
module GameIO.Audio.Local
  ( Audio
  , withAudio
  , openAudio
  , closeAudio
  , readAudioFormat
  , readAudioFrame
  , readAudioStats
  , requestAudioDeviceRefresh
  , Voice
  , playSound
  , playSoundAt
  , readVoice
  , setVoiceMix
  , setVoiceGain
  , setVoicePan
  , pauseVoice
  , resumeVoice
  , stopVoice
  , stopVoiceAt
  , seekVoice
  , AudioStream
  , openStream
  , streamVoice
  , streamWritableFrames
  , writeStream
  , resetStream
  , finishStream
  ) where

import Control.Concurrent
  ( MVar
  , forkOSWithUnmask
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , rtsSupportsBoundThreads
  , tryPutMVar
  , withMVar
  )
import Control.Exception
  ( SomeException
  , bracket
  , displayException
  , finally
  , fromException
  , mask
  , mask_
  , onException
  , throwIO
  , try
  )
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GameIO.Audio.Mixer
import GameIO.Audio.PCM
import GameIO.Audio.Types
import GameIO.Audio.WASAPI qualified as WASAPI

data Audio = Audio
  { audioMixer :: !Mixer
  , audioControlEvents :: !(MVar (Maybe ControlEvents))
  , audioFinished :: !(MVar ())
  , audioStatsRef :: !(IORef AudioStats)
  , audioInitialFormat :: !AudioFormat
  , audioFormatRef :: !(IORef (Maybe AudioFormat))
  }

data ControlEvents = ControlEvents !WASAPI.Event !WASAPI.Event

-- | Open the default render endpoint on an audio-owned bound OS thread.
withAudio :: AudioConfig -> (Audio -> IO a) -> IO a
withAudio config = bracket (openAudio config) closeAudio

openAudio :: AudioConfig -> IO Audio
openAudio config@(AudioConfig rate _ maxVoices) = mask $ \restore -> do
  unless rtsSupportsBoundThreads (throwIO AudioNeedsThreadedRTS)
  mixer <- newMixer rate maxVoices
  status <- newIORef emptyAudioStats
  latestFormat <- newIORef Nothing
  ready <- newEmptyMVar
  finished <- newEmptyMVar
  shutdown <- WASAPI.newEvent
  refresh <- WASAPI.newEvent `onException` WASAPI.closeEvent shutdown
  let events = ControlEvents shutdown refresh
  controls <- newMVar (Just events)
  let closeEvents = WASAPI.closeEvent refresh `finally` WASAPI.closeEvent shutdown
      worker unmask = do
        result <- try (unmask (deviceWorker config mixer events status latestFormat ready))
        closeMixer mixer
        let (finalStatus, startupError) = case result of
              Right () -> (AudioClosed, AudioNotRunning)
              Left (exception :: SomeException) ->
                let issue = maybe (AudioWorkerFailed (displayException exception)) id (fromException exception)
                in (AudioFailed issue, issue)
        modifyIORef' status (\current -> current {audioStatus = finalStatus})
        void (tryPutMVar ready (Left startupError))
        modifyMVar_ controls (\_ -> closeEvents >> pure Nothing)
        putMVar finished ()
  void (forkOSWithUnmask (\unmask -> worker unmask)) `onException` closeEvents
  opened <- restore (readMVar ready) `onException` stopWorker mixer controls
  case opened of
    Left issue -> readMVar finished >> throwIO issue
    Right format -> pure (Audio mixer controls finished status format latestFormat)

-- | Stop scheduling, wake the worker and wait for COM and the endpoint to close.
-- This may be called repeatedly and concurrently.
closeAudio :: Audio -> IO ()
closeAudio audio = mask_ $ do
  stopWorker (audioMixer audio) (audioControlEvents audio)
  readMVar (audioFinished audio)

stopWorker :: Mixer -> MVar (Maybe ControlEvents) -> IO ()
stopWorker mixer controls = do
  closeMixer mixer
  withMVar controls $ \events ->
    forM_ events $ \(ControlEvents shutdown _) -> WASAPI.signalEvent shutdown

readAudioFormat :: Audio -> IO AudioFormat
readAudioFormat audio = maybe (audioInitialFormat audio) id <$> readIORef (audioFormatRef audio)

-- | Earliest frame that can still accept a scheduled command at the instant of
-- observation. The mixer can reserve more frames before the command arrives.
readAudioFrame :: Audio -> IO AudioFrame
readAudioFrame = nextAudioFrame . audioMixer

readAudioStats :: Audio -> IO AudioStats
readAudioStats = readIORef . audioStatsRef

-- | Reopen the current default endpoint on the worker. Voices, their PCM
-- positions and scheduled output-frame commands survive the device transition.
-- Observe AudioStats to see when the replacement endpoint is running.
requestAudioDeviceRefresh :: Audio -> IO ()
requestAudioDeviceRefresh audio = withMVar (audioControlEvents audio) $ \events ->
  case events of
    Nothing -> throwIO AudioNotRunning
    Just (ControlEvents _ refresh) -> WASAPI.signalEvent refresh

playSound :: Audio -> PCM -> VoiceOptions -> IO Voice
playSound = playClip . audioMixer

-- | Play at an absolute output frame. A command arriving after that frame has
-- been reserved fails with AudioFrameAlreadyReserved.
playSoundAt :: Audio -> AudioFrame -> PCM -> VoiceOptions -> IO Voice
playSoundAt audio frame = playClipAt (audioMixer audio) (Just frame)

-- | Attach a bounded PCM queue as a paused voice. Queue the initial data, then
-- call resumeVoice on the stream's voice. A producer may run on any thread.
openStream :: Audio -> Int -> Channels -> Int -> IO AudioStream
openStream = openMixerStream . audioMixer

deviceWorker
  :: AudioConfig -> Mixer -> ControlEvents -> IORef AudioStats
  -> IORef (Maybe AudioFormat) -> MVar (Either AudioError AudioFormat) -> IO ()
deviceWorker config mixer (ControlEvents shutdown refresh) stats latestFormat ready = run
  where
    run = do
      result <- try connect
      case result of
        Right WASAPI.Shutdown -> pure ()
        Right _ -> run
        Left issue -> do
          opened <- readIORef latestFormat
          case opened of
            Just _ | WASAPI.recoverableDeviceError issue -> do
              modifyIORef' stats (\current -> current {audioStatus = AudioRecovering issue})
              WASAPI.withControlWait shutdown refresh $ \wait -> do
                wake <- wait
                unless (wake == WASAPI.Shutdown) run
            _ -> throwIO issue

    connect = WASAPI.withRenderDevice config $ \device ->
      WASAPI.withStartedDevice device $ do
        let format = WASAPI.deviceFormat device
        previous <- readIORef latestFormat
        writeIORef latestFormat (Just format)
        modifyIORef' stats $ \current -> current
          { audioStatus = AudioRunning
          , deviceRestarts = deviceRestarts current + maybe 0 (const 1) previous
          }
        void (tryPutMVar ready (Right format))
        WASAPI.withDeviceWait shutdown refresh device $ \wait -> do
          started <- getMonotonicTimeNSec
          runRenderLoop mixer stats device wait started

runRenderLoop :: Mixer -> IORef AudioStats -> WASAPI.RenderDevice -> IO WASAPI.Wake -> Word64 -> IO WASAPI.Wake
runRenderLoop mixer stats device wait = service
  where
    format = WASAPI.deviceFormat device
    capacity = audioDeviceBufferFrames format
    rate = fromIntegral (audioSampleRate format)
    period = audioDevicePeriodNs format

    service !previousWake = do
      wake <- wait
      case wake of
        WASAPI.Shutdown -> pure wake
        WASAPI.Refresh -> pure wake
        _ -> do
          started <- getMonotonicTimeNSec
          padding <- WASAPI.devicePadding device
          let available = capacity - padding
          when (available > 0) $ do
            MixReport starved <- WASAPI.withOutputPacket device available $ \pointer ->
              renderMixer mixer pointer available
            finished <- getMonotonicTimeNSec
            let work = finished - started
                paddingNs = fromIntegral padding * 1000000000 `quot` rate
                wakeGap = started - previousWake
            modifyIORef' stats $ \current -> current
              { submittedPackets = submittedPackets current + 1
              , submittedFrames = submittedFrames current + fromIntegral available
              , emptyDeviceQueueEvents = emptyDeviceQueueEvents current + count (padding == 0)
              , exceededPaddingDeadline = exceededPaddingDeadline current + count (padding > 0 && work > paddingNs)
              , exceededDevicePeriod = exceededDevicePeriod current + count (work > period)
              , maximumServiceNs = max work (maximumServiceNs current)
              , maximumWakeGapNs = max wakeGap (maximumWakeGapNs current)
              , streamStarvedFrames = streamStarvedFrames current + starved
              , lastDevicePaddingFrames = padding
              }
          service started

    count matched = if matched then 1 else 0
