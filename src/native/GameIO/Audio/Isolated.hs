{-# LANGUAGE LambdaCase #-}

module GameIO.Audio.Isolated
  ( RemoteAudio
  , RemoteVoice
  , RemoteSound
  , openRemoteAudio
  , closeRemoteAudio
  , remoteFormat
  , remoteFrame
  , remoteStats
  , remotePid
  , remoteRefresh
  , remotePlay
  , remotePrepare
  , remotePlayPrepared
  , remoteOpenStream
  , remoteWrite
  , remoteAvailable
  , remoteVoiceSnapshot
  , remoteVoiceControl
  , runAudioWorker
  ) where

import Control.Concurrent (MVar, forkIOWithUnmask, modifyMVarMasked, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception
  ( IOException
  , SomeAsyncException
  , SomeException
  , catch
  , displayException
  , finally
  , fromException
  , mask
  , mask_
  , onException
  , throwIO
  , try
  )
import Control.Monad (forM_, unless, void)
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Word (Word64)
import Foreign.Concurrent qualified as Foreign
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (nullPtr)
import GameIO.Audio.Local qualified as Local
import GameIO.Audio.PCM (PCM)
import GameIO.Audio.Protocol
import GameIO.Audio.Types
import System.Exit (ExitCode (..))
import System.IO
  ( BufferMode (NoBuffering)
  , Handle
  , hClose
  , hFlush
  , hSetBinaryMode
  , hSetBuffering
  , stdin
  , stdout
  )
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , createProcess
  , getPid
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

data Child = Child !Handle !Handle !ProcessHandle

data Connection = Connected !Child !Word64 | Disconnected

data RemoteAudio = RemoteAudio
  { connection :: !(MVar Connection)
  , releaseQueue :: !(IORef [Int])
  , cachedFormat :: !(IORef AudioFormat)
  , cachedStats :: !(IORef AudioStats)
  , remotePid :: !(Maybe Int)
  }

data RemoteHandle = RemoteHandle !RemoteAudio !Int !(ForeignPtr ())

newtype RemoteVoice = RemoteVoice RemoteHandle

newtype RemoteSound = RemoteSound RemoteHandle

openRemoteAudio :: FilePath -> AudioConfig -> IO RemoteAudio
openRemoteAudio executable config = mask $ \restore -> do
  child@(Child _ _ process) <- startChild executable
  let configure = do
        reply <- exchange child 1 [] (Configure config) Nothing >>= either throwIO pure
        case reply of
          Format format -> pure format
          _ -> unexpectedReply
  let open = do
        format <- restore configure
        RemoteAudio <$> newMVar (Connected child 2) <*> newIORef [] <*> newIORef format
          <*> newIORef emptyAudioStats <*> (fmap fromIntegral <$> getPid process)
  (open `onException` abortChild child)
    `catch` (\(issue :: IOException) -> throwIO (AudioTransportFailure (displayException issue)))

startChild :: FilePath -> IO Child
startChild executable = do
  let settings = (proc executable [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        , close_fds = True
        , detach_console = True
        }
  started <- try (createProcess settings)
  case started of
    Left (issue :: IOException) -> throwIO (AudioTransportFailure (displayException issue))
    Right (Just send, Just receive, _, process) -> do
      let child = Child send receive process
      mapM_ preparePipe [send, receive] `onException` abortChild child
      pure child
    Right (send, receive, _, process) -> do
      ignoreIO (terminateProcess process)
      forM_ send (ignoreIO . hClose)
      forM_ receive (ignoreIO . hClose)
      void (waitForProcess process)
      throwIO (AudioTransportFailure "The audio worker did not provide two pipes")

preparePipe :: Handle -> IO ()
preparePipe handle = hSetBinaryMode handle True >> hSetBuffering handle NoBuffering

closeRemoteAudio :: RemoteAudio -> IO ()
closeRemoteAudio audio = mask $ \restore -> do
  outcome <- modifyMVarMasked (connection audio) $ \case
    Disconnected -> pure (Disconnected, Right ())
    Connected child@(Child _ _ process) serial -> do
      stopped <- try $ restore $ do
        reply <- exchange child serial [] Shutdown Nothing >>= either throwIO pure
        requireAck reply
        closePipes child
        exited <- within 3000000 (waitForProcess process)
        case exited of
          Just ExitSuccess -> pure ()
          Just status -> throwIO (AudioTransportFailure ("Worker exited with " ++ show status))
          Nothing -> throwIO (AudioTransportFailure "Worker did not exit after shutdown acknowledgement")
      case stopped of
        Right () -> setRemoteStatus audio AudioClosed
        Left issue -> do
          abortChild child
          setRemoteStatus audio (AudioFailed (transportIssue issue))
      pure (Disconnected, stopped)
  case outcome of
    Left issue | Just (_ :: SomeAsyncException) <- fromException issue -> throwIO issue
    _ -> pure ()

abortChild :: Child -> IO ()
abortChild child@(Child _ _ process) = do
  ignoreIO (terminateProcess process)
  closePipes child
  void (within 3000000 (waitForProcess process))

closePipes :: Child -> IO ()
closePipes (Child send receive _) = ignoreIO (hClose send) >> ignoreIO (hClose receive)

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` (\(_ :: IOException) -> pure ())

remoteFormat :: RemoteAudio -> IO AudioFormat
remoteFormat audio = (remoteCall audio QueryFormat Nothing >>= \case
  Format format -> writeIORef (cachedFormat audio) format >> pure format
  _ -> unexpectedReply)
  `catch` (\issue -> case issue of
    AudioNotRunning -> readIORef (cachedFormat audio)
    _ -> throwIO issue)

remoteFrame :: RemoteAudio -> IO AudioFrame
remoteFrame audio = remoteCall audio QueryFrame Nothing >>= \case
  Frame frame -> pure frame
  _ -> unexpectedReply

remoteStats :: RemoteAudio -> IO AudioStats
remoteStats audio =
  (remoteCall audio QueryStats Nothing >>= \case
    Statistics stats -> do
      atomicModifyIORef' (cachedStats audio) $ \old -> case audioStatus old of
        AudioClosed -> (old, ())
        AudioFailed _ -> (old, ())
        _ -> (stats, ())
      readIORef (cachedStats audio)
    _ -> unexpectedReply)
  `catch` (\(issue :: AudioError) -> do
    current <- readIORef (cachedStats audio)
    case audioStatus current of
      AudioClosed -> pure current
      AudioFailed _ -> pure current
      _ -> throwIO issue)

remoteRefresh :: RemoteAudio -> IO ()
remoteRefresh audio = remoteCall audio RefreshEndpoint Nothing >>= requireAck

remotePlay :: RemoteAudio -> Maybe AudioFrame -> PCM -> VoiceOptions -> IO RemoteVoice
remotePlay audio start pcm options = mask_ $ do
  header <- describePayload pcm
  reply <- remoteCall audio (Play start options header) (Just pcm)
  RemoteVoice <$> registerRemoteHandle audio reply

remotePrepare :: RemoteAudio -> PCM -> IO RemoteSound
remotePrepare audio pcm = mask_ $ do
  header <- describePayload pcm
  reply <- remoteCall audio (Prepare header) (Just pcm)
  RemoteSound <$> registerRemoteHandle audio reply

remotePlayPrepared :: RemoteSound -> Maybe AudioFrame -> VoiceOptions -> IO RemoteVoice
remotePlayPrepared (RemoteSound (RemoteHandle audio identifier lifetime)) start options =
  mask_ $ withForeignPtr lifetime $ \_ -> do
    reply <- remoteCall audio (PlayPrepared identifier start options) Nothing
    RemoteVoice <$> registerRemoteHandle audio reply

remoteOpenStream :: RemoteAudio -> Int -> Channels -> Int -> IO RemoteVoice
remoteOpenStream audio rate channels capacity = mask_ $ do
  reply <- remoteCall audio (OpenStream rate channels capacity) Nothing
  RemoteVoice <$> registerRemoteHandle audio reply

registerRemoteHandle :: RemoteAudio -> Reply -> IO RemoteHandle
registerRemoteHandle audio (NewHandle identifier) = do
  let release = atomicModifyIORef' (releaseQueue audio) (\queued -> (identifier : queued, ()))
  lifetime <- Foreign.newForeignPtr nullPtr release
  pure (RemoteHandle audio identifier lifetime)
registerRemoteHandle _ _ = unexpectedReply

remoteVoiceSnapshot :: RemoteVoice -> IO VoiceSnapshot
remoteVoiceSnapshot voice = voiceCall voice QueryVoice >>= \case
  Snapshot snapshot -> pure snapshot
  _ -> unexpectedReply

remoteVoiceControl :: RemoteVoice -> VoiceCommand -> IO ()
remoteVoiceControl voice operation = voiceCall voice operation >>= requireAck

remoteAvailable :: RemoteVoice -> IO Int
remoteAvailable voice = voiceCall voice Available >>= replyCount

remoteWrite :: RemoteVoice -> PCM -> IO Int
remoteWrite (RemoteVoice (RemoteHandle audio identifier lifetime)) pcm = withForeignPtr lifetime $ \_ -> do
  header <- describePayload pcm
  remoteCall audio (WriteStream identifier header) (Just pcm) >>= replyCount

voiceCall :: RemoteVoice -> VoiceCommand -> IO Reply
voiceCall (RemoteVoice (RemoteHandle audio identifier lifetime)) operation = withForeignPtr lifetime $ \_ ->
  remoteCall audio (ControlVoice identifier operation) Nothing

replyCount :: Reply -> IO Int
replyCount (Count value) = pure value
replyCount _ = unexpectedReply

requireAck :: Reply -> IO ()
requireAck Acknowledged = pure ()
requireAck _ = unexpectedReply

unexpectedReply :: IO a
unexpectedReply = throwIO (AudioTransportFailure "Unexpected audio worker acknowledgement")

remoteCall :: RemoteAudio -> Command -> Maybe PCM -> IO Reply
remoteCall audio command payload = mask $ \restore -> do
  outcome <- modifyMVarMasked (connection audio) $ \case
    Disconnected -> pure (Disconnected, Right (Left AudioNotRunning))
    Connected child serial -> do
      released <- atomicModifyIORef' (releaseQueue audio) (\queued -> ([], queued))
      answer <- try (restore (exchange child serial released command payload))
      case answer of
        Right reply -> pure (Connected child (serial + 1), Right reply)
        Left exception -> do
          abortChild child
          setRemoteStatus audio (AudioFailed (transportIssue exception))
          pure (Disconnected, Left exception)
  case outcome of
    Right result -> either throwIO pure result
    Left exception
      | Just (_ :: SomeAsyncException) <- fromException exception -> throwIO exception
      | otherwise -> throwIO (transportIssue exception)

transportIssue :: SomeException -> AudioError
transportIssue exception =
  maybe (AudioTransportFailure (displayException exception)) id (fromException exception)

setRemoteStatus :: RemoteAudio -> AudioStatus -> IO ()
setRemoteStatus audio status =
  atomicModifyIORef' (cachedStats audio) (\stats -> (stats {audioStatus = status}, ()))

exchange :: Child -> Word64 -> [Int] -> Command -> Maybe PCM -> IO (Either AudioError Reply)
exchange (Child send receive _) serial released command payload = do
  response <- within 5000000 $ do
    sendMessage send (Request serial released command)
    forM_ payload (sendPCM send)
    hFlush send
    receiveMessage receive >>= \case
      Just (Answer acknowledged reply) | acknowledged == serial -> pure reply
      Just _ -> throwIO (AudioTransportFailure "Out-of-order acknowledgement")
      Nothing -> throwIO (AudioTransportFailure "Audio worker pipe closed before acknowledgement")
  maybe (throwIO (AudioTransportFailure "Audio worker command timed out after 5 seconds")) pure response

-- A caller waiting for a reply can time out even when a pipe system call is
-- blocked. The caller terminates the child on failure, releasing its peer.
within :: Int -> IO a -> IO (Maybe a)
within microseconds operation = do
  result <- newEmptyMVar
  void $ forkIOWithUnmask $ \unmask ->
    try (unmask operation) >>= putMVar result
  answer <- timeout microseconds (takeMVar result)
  traverse (either (throwIO :: SomeException -> IO a) pure) answer

data HeldVoice = HeldVoice !Local.Voice !(Maybe Local.AudioStream)

data HeldResource = VoiceHandle !HeldVoice | SoundHandle !PCM

data WorkerState = WorkerState !Int !(IntMap.IntMap HeldResource)

-- | Serve a versioned PCM/control connection over the inherited stdin/stdout
-- pipes. EOF or a broken pipe releases the output device.
runAudioWorker :: IO ()
runAudioWorker = do
  mapM_ preparePipe [stdin, stdout]
  receiveMessage stdin >>= \case
    Nothing -> pure ()
    Just (Request 1 [] (Configure (AudioConfig rate buffer voices))) -> mask $ \restore -> do
      opened <- try (either throwIO Local.openAudio (makeAudioConfig rate buffer voices))
      case opened of
        Left issue -> reply 1 (Left issue)
        Right audio -> restore (do
          format <- Local.readAudioFormat audio
          reply 1 (Right (Format format))
          serve audio 2 (WorkerState 0 IntMap.empty))
          `finally` Local.closeAudio audio
    Just _ -> throwIO (AudioTransportFailure "Expected initial audio configuration")
  where
    reply serial result = sendMessage stdout (Answer serial result) >> hFlush stdout

    serve audio !expected state = receiveMessage stdin >>= \case
      Nothing -> pure ()
      Just (Request serial released command) -> do
        unless (serial == expected && length released <= remoteHandleLimit) $
          throwIO (AudioTransportFailure "Invalid request sequence or release count")
        let retained = releaseHandles released state
        case command of
          Shutdown -> Local.closeAudio audio >> reply serial (Right Acknowledged)
          _ -> do
            -- A PCM body is fully consumed even when its subsequent command
            -- will be refused by the mixer or the handle table.
            payload <- case command of
              Play _ _ header -> Just <$> receivePCM stdin header
              Prepare header -> Just <$> receivePCM stdin header
              WriteStream _ header -> Just <$> receivePCM stdin header
              _ -> pure Nothing
            result <- try (dispatch audio retained command payload)
            case result of
              Left issue -> reply serial (Left issue) >> serve audio (expected + 1) retained
              Right (value, updated) -> reply serial (Right value) >> serve audio (expected + 1) updated

releaseHandles :: [Int] -> WorkerState -> WorkerState
releaseHandles handles (WorkerState next voices) =
  WorkerState next (List.foldl' (flip IntMap.delete) voices handles)

dispatch :: Local.Audio -> WorkerState -> Command -> Maybe PCM -> IO (Reply, WorkerState)
dispatch audio state@(WorkerState next resources) command payload = case (command, payload) of
  (QueryFormat, _) -> unchanged (Format <$> Local.readAudioFormat audio)
  (QueryFrame, _) -> unchanged (Frame <$> Local.readAudioFrame audio)
  (QueryStats, _) -> unchanged (Statistics <$> Local.readAudioStats audio)
  (RefreshEndpoint, _) -> unchanged (Local.requestAudioDeviceRefresh audio >> pure Acknowledged)
  (Play frame options _, Just pcm) -> startClip frame options pcm
  (Prepare _, Just pcm) -> admitHandle >> registered (SoundHandle pcm)
  (PlayPrepared identifier frame options, _) -> case IntMap.lookup identifier resources of
    Just (SoundHandle pcm) -> startClip frame options pcm
    _ -> throwIO AudioAssetUnavailable
  (OpenStream rate channels capacity, _) -> do
    admitHandle
    stream <- Local.openStream audio rate channels capacity
    registered (VoiceHandle (HeldVoice (Local.streamVoice stream) (Just stream)))
  (WriteStream identifier _, Just pcm) -> unchanged $ do
    HeldVoice _ stream <- lookupVoice identifier
    Count <$> withStream stream (`Local.writeStream` pcm)
  (ControlVoice identifier operation, _) -> unchanged (lookupVoice identifier >>= applyVoice operation)
  _ -> throwIO (AudioTransportFailure "Unexpected command in an open audio session")
  where
    unchanged action = (, state) <$> action
    registered resource = pure (NewHandle next, WorkerState (next + 1) (IntMap.insert next resource resources))
    lookupVoice identifier = case IntMap.lookup identifier resources of
      Just (VoiceHandle voice) -> pure voice
      _ -> throwIO VoiceInactive
    admitHandle = unless (IntMap.size resources < remoteHandleLimit) (throwIO AudioWorkerHandleLimit)
    startClip frame (VoiceOptions mode gain pan) pcm = do
      admitHandle
      options <- either throwIO pure (voiceOptions mode gain pan)
      voice <- case frame of
        Nothing -> Local.playSound audio pcm options
        Just at -> Local.playSoundAt audio at pcm options
      registered (VoiceHandle (HeldVoice voice Nothing))

applyVoice :: VoiceCommand -> HeldVoice -> IO Reply
applyVoice operation (HeldVoice voice stream) = case operation of
  QueryVoice -> Snapshot <$> Local.readVoice voice
  SetGain value -> acknowledge (Local.setVoiceGain voice value)
  SetPan value -> acknowledge (Local.setVoicePan voice value)
  SetMix gain pan -> acknowledge (Local.setVoiceMix voice gain pan)
  Pause -> acknowledge (Local.pauseVoice voice)
  Resume -> acknowledge (Local.resumeVoice voice)
  Stop -> acknowledge (Local.stopVoice voice)
  StopAt frame -> acknowledge (Local.stopVoiceAt voice frame)
  Seek frame -> acknowledge (Local.seekVoice voice frame)
  Available -> Count <$> withStream stream Local.streamWritableFrames
  Reset -> acknowledge (withStream stream Local.resetStream)
  Finish -> acknowledge (withStream stream Local.finishStream)
  where
    acknowledge action = action >> pure Acknowledged

withStream :: Maybe Local.AudioStream -> (Local.AudioStream -> IO a) -> IO a
withStream stream action = maybe (throwIO VoiceNotSeekable) action stream
