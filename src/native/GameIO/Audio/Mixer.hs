{-# LANGUAGE MultiWayIf #-}

module GameIO.Audio.Mixer
  ( Mixer
  , newMixer
  , closeMixer
  , nextAudioFrame
  , Voice
  , playClip
  , playClipAt
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
  , openMixerStream
  , streamVoice
  , streamWritableFrames
  , writeStream
  , resetStream
  , finishStream
  , MixReport (..)
  , renderMixer
  ) where

import Control.Exception (mask_, throwIO)
import Control.Monad (foldM, unless, when)
import Data.Foldable (traverse_)
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Word (Word64)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GameIO.Audio.PCM
import GameIO.Audio.Types

data Mixer = Mixer !Int !Int !(IORef Registry)

data Registry = Registry
  { registryAccepting :: !Bool
  , registryNextIdentifier :: !Int
  , registryNextFrame :: !AudioFrame
  , registryVoices :: !(IntMap.IntMap ScheduledVoice)
  }

data ScheduledVoice = ScheduledVoice !AudioFrame !(Maybe AudioFrame) !Voice

data Voice = Voice !(IORef Registry) !Int !Int !Channels !(IORef Playback)

data Playback = Playback
  { playbackStatus :: !VoiceStatus
  , playbackGain :: !Float
  , playbackPan :: !Float
  , playbackCursor :: !Cursor
  , playbackRevision :: !Word64
  , playbackSource :: !Source
  }

-- Fractional position is an integer remainder in output-rate units.
data Cursor = Cursor !Int !Int

data Source = Clip !PCM !LoopMode | Stream !StreamQueue | Retired

data StreamQueue = StreamQueue
  { queueCapacity :: !Int
  , queueConsumedThrough :: !Int
  , queueWrittenThrough :: !Int
  , queueChunks :: !(Seq Chunk)
  , queueEnd :: !StreamEnd
  }

data Chunk = Chunk !Int !PCM

data StreamEnd = MoreChunks | EndOfStream
  deriving (Eq)

newtype AudioStream = AudioStream Voice

newMixer :: Int -> Int -> IO Mixer
newMixer rate maxVoices =
  Mixer rate maxVoices <$> newIORef (Registry True 0 (AudioFrame 0) IntMap.empty)

closeMixer :: Mixer -> IO ()
closeMixer (Mixer _ _ registry) = mask_ $ do
  voices <- atomicModifyIORef' registry $ \current ->
    (current {registryAccepting = False, registryVoices = IntMap.empty}, registryVoices current)
  traverse_ (\(ScheduledVoice _ _ voice) -> retireVoice VoiceClosed voice) voices

nextAudioFrame :: Mixer -> IO AudioFrame
nextAudioFrame (Mixer _ _ registry) = registryNextFrame <$> readIORef registry

playClip :: Mixer -> PCM -> VoiceOptions -> IO Voice
playClip mixer = playClipAt mixer Nothing

playClipAt :: Mixer -> Maybe AudioFrame -> PCM -> VoiceOptions -> IO Voice
playClipAt mixer start pcm (VoiceOptions loopMode gain pan) =
  registerVoice mixer start (pcmSampleRate pcm) (pcmChannels pcm)
    (Playback VoicePlaying gain pan (Cursor 0 0) 0 (Clip pcm loopMode))

-- | Streams start paused so the producer can queue samples before playback.
openMixerStream :: Mixer -> Int -> Channels -> Int -> IO AudioStream
openMixerStream mixer rate channels capacity = do
  either throwIO pure (validateSampleRate rate)
  unless (capacity > 0 && capacity <= maxBound `quot` (4 * channelCount channels)) $
    throwIO (InvalidFrameCount capacity)
  let source = Stream (StreamQueue capacity 0 0 Seq.empty MoreChunks)
      playback = Playback VoicePaused 1 0 (Cursor 0 0) 0 source
  AudioStream <$> registerVoice mixer Nothing rate channels playback

registerVoice :: Mixer -> Maybe AudioFrame -> Int -> Channels -> Playback -> IO Voice
registerVoice (Mixer _ maxVoices registry) start rate channels playback = do
  playbackRef <- newIORef playback
  result <- atomicModifyIORef' registry $ \current ->
    let earliest = registryNextFrame current
        startFrame = maybe earliest id start
        voices = registryVoices current
        identifier = registryNextIdentifier current
    in if
      | not (registryAccepting current) -> (current, Left AudioNotRunning)
      | IntMap.size voices >= maxVoices -> (current, Left VoiceLimitReached)
      | startFrame < earliest -> (current, Left (AudioFrameAlreadyReserved startFrame earliest))
      | otherwise ->
          let voice = Voice registry identifier rate channels playbackRef
              updated = current
                { registryNextIdentifier = identifier + 1
                , registryVoices = IntMap.insert identifier (ScheduledVoice startFrame Nothing voice) voices
                }
          in (updated, Right voice)
  either throwIO pure result

readVoice :: Voice -> IO VoiceSnapshot
readVoice (Voice _ _ rate _ ref) = do
  playback <- readIORef ref
  let Cursor frame _ = playbackCursor playback
  pure (VoiceSnapshot (playbackStatus playback) frame rate (playbackGain playback) (playbackPan playback))

setVoiceMix :: Voice -> Float -> Float -> IO ()
setVoiceMix voice gain pan = do
  either throwIO pure (checkGain gain >> checkPan pan)
  updateLive voice (\playback -> Right (playback {playbackGain = gain, playbackPan = pan}, ()))

setVoiceGain :: Voice -> Float -> IO ()
setVoiceGain voice gain = do
  either throwIO pure (checkGain gain)
  updateLive voice (\playback -> Right (playback {playbackGain = gain}, ()))

setVoicePan :: Voice -> Float -> IO ()
setVoicePan voice pan = do
  either throwIO pure (checkPan pan)
  updateLive voice (\playback -> Right (playback {playbackPan = pan}, ()))

pauseVoice :: Voice -> IO ()
pauseVoice voice =
  updateLive voice (\playback -> Right (playback {playbackStatus = VoicePaused}, ()))

resumeVoice :: Voice -> IO ()
resumeVoice voice =
  updateLive voice (\playback -> Right (playback {playbackStatus = VoicePlaying}, ()))

-- | Idempotently retire a voice. Any already mixed samples remain queued.
stopVoice :: Voice -> IO ()
stopVoice = retireVoice VoiceStopped

-- | Exclude the requested output frame and all later frames. A frame reserved
-- for a device packet has already passed the admission boundary.
stopVoiceAt :: Voice -> AudioFrame -> IO ()
stopVoiceAt (Voice registry identifier _ _ _) frame = do
  result <- atomicModifyIORef' registry $ \current ->
    let earliest = registryNextFrame current
        voices = registryVoices current
    in case IntMap.lookup identifier voices of
      Nothing -> (current, Left VoiceInactive)
      Just (ScheduledVoice start _ voice)
        | frame < earliest -> (current, Left (AudioFrameAlreadyReserved frame earliest))
        | otherwise ->
            (current {registryVoices = IntMap.insert identifier (ScheduledVoice start (Just frame) voice) voices}, Right ())
  either throwIO pure result

-- | Seek a live in-memory sound in source frames. A stream uses resetStream.
seekVoice :: Voice -> Int -> IO ()
seekVoice voice frame = updateLive voice $ \playback -> case playbackSource playback of
  Clip pcm _
    | frame >= 0 && frame < pcmFrames pcm ->
        let reset = playback
              { playbackCursor = Cursor frame 0
              , playbackRevision = playbackRevision playback + 1
              }
        in Right (reset, ())
    | otherwise -> Left (VoicePositionOutOfRange frame)
  _ -> Left VoiceNotSeekable

updateLive :: Voice -> (Playback -> Either AudioError (Playback, a)) -> IO a
updateLive (Voice _ _ _ _ ref) change = do
  result <- atomicModifyIORef' ref $ \playback -> case playbackSource playback of
    Retired -> (playback, Left VoiceInactive)
    _ -> case change playback of
      Left issue -> (playback, Left issue)
      Right (updated, value) -> (updated, Right value)
  either throwIO pure result

retireVoice :: VoiceStatus -> Voice -> IO ()
retireVoice status voice@(Voice _ _ _ _ ref) = mask_ $ do
  atomicModifyIORef' ref $ \playback -> case playbackSource playback of
    Retired -> (playback, ())
    _ -> (retired status playback, ())
  unregisterVoice voice

retired :: VoiceStatus -> Playback -> Playback
retired status playback = playback {playbackStatus = status, playbackSource = Retired}

unregisterVoice :: Voice -> IO ()
unregisterVoice (Voice registry identifier _ _ _) =
  atomicModifyIORef' registry $ \current ->
    (current {registryVoices = IntMap.delete identifier (registryVoices current)}, ())

streamVoice :: AudioStream -> Voice
streamVoice (AudioStream voice) = voice

streamWritableFrames :: AudioStream -> IO Int
streamWritableFrames (AudioStream (Voice _ _ _ _ ref)) = do
  playback <- readIORef ref
  case playbackSource playback of
    Stream queue | queueEnd queue == MoreChunks -> pure (queueAvailable queue)
    Stream _ -> pure 0
    _ -> throwIO VoiceInactive

queueAvailable :: StreamQueue -> Int
queueAvailable queue = queueCapacity queue - (queueWrittenThrough queue - queueConsumedThrough queue)

-- | Accept a complete PCM block or return zero when the bounded queue is full.
-- Multiple producers are serialized in the order in which writes commit.
writeStream :: AudioStream -> PCM -> IO Int
writeStream (AudioStream voice@(Voice _ _ rate channels _)) pcm = do
  when (pcmSampleRate pcm /= rate || pcmChannels pcm /= channels) (throwIO StreamFormatMismatch)
  updateLive voice $ \playback -> case playbackSource playback of
    Stream queue
      | queueEnd queue == EndOfStream -> Left StreamFinished
      | pcmFrames pcm > queueAvailable queue -> Right (playback, 0)
      | otherwise ->
          let start = queueWrittenThrough queue
              updated = queue
                { queueWrittenThrough = start + pcmFrames pcm
                , queueChunks = queueChunks queue |> Chunk start pcm
                }
          in Right (playback {playbackSource = Stream updated}, pcmFrames pcm)
    _ -> Left VoiceInactive

-- | Discard pending source frames and start a new source clock at zero.
-- In-flight packets keep their samples, and a paused stream remains paused.
resetStream :: AudioStream -> IO ()
resetStream (AudioStream voice) = updateLive voice $ \playback -> case playbackSource playback of
  Stream queue ->
    let reset = StreamQueue (queueCapacity queue) 0 0 Seq.empty MoreChunks
    in Right (playback
      { playbackCursor = Cursor 0 0
      , playbackRevision = playbackRevision playback + 1
      , playbackSource = Stream reset
      }, ())
  _ -> Left VoiceInactive

-- | Seal the stream. The voice ends after the last source frame is mixed.
finishStream :: AudioStream -> IO ()
finishStream (AudioStream voice) = updateLive voice $ \playback -> case playbackSource playback of
  Stream queue -> Right (playback {playbackSource = Stream (queue {queueEnd = EndOfStream})}, ())
  _ -> Left VoiceInactive

newtype MixReport = MixReport { mixStarvedStreamFrames :: Word64 }
  deriving (Eq, Show)

renderMixer :: Mixer -> Ptr Float -> Int -> IO MixReport
renderMixer (Mixer rate _ registry) output frames = do
  (begin, end, voices) <- atomicModifyIORef' registry $ \current ->
    let begin = registryNextFrame current
        end = addFrames begin frames
    in (current {registryNextFrame = end}, (begin, end, registryVoices current))
  fillBytes output 0 (frames * 8)
  starvation <- foldM (mixScheduled rate output begin end) 0 (IntMap.elems voices)
  saturateBlock output (frames * 2)
  pure (MixReport starvation)

addFrames :: AudioFrame -> Int -> AudioFrame
addFrames (AudioFrame frame) count = AudioFrame (frame + fromIntegral count)

frameOffset :: AudioFrame -> AudioFrame -> Int
frameOffset (AudioFrame begin) (AudioFrame frame) = fromIntegral (frame - begin)

mixScheduled :: Int -> Ptr Float -> AudioFrame -> AudioFrame -> Word64 -> ScheduledVoice -> IO Word64
mixScheduled rate output begin end !starvation (ScheduledVoice start stop voice) = do
  let from = max start begin
      endExclusive = maybe end (min end) stop
  starved <- if from < endExclusive
    then mixVoice rate output (frameOffset begin from) (frameOffset begin endExclusive) voice
    else pure 0
  when (maybe False (<= end) stop) (retireVoice VoiceStopped voice)
  pure (starvation + starved)

mixVoice :: Int -> Ptr Float -> Int -> Int -> Voice -> IO Word64
mixVoice outputRate output from endExclusive voice@(Voice _ _ sourceRate _ ref) = do
  playback <- readIORef ref
  if playbackStatus playback /= VoicePlaying
    then pure 0
    else do
      let gain = playbackGain playback
          pan = playbackPan playback
          leftGain = gain * (1 - max 0 pan)
          rightGain = gain * (1 + min 0 pan)
          spanMixer = mixSpan output outputRate sourceRate leftGain rightGain endExclusive
          cursor = playbackCursor playback
          revision = playbackRevision playback
      case playbackSource playback of
        Clip pcm loopMode -> do
          let boundary = if loopMode == Loop then LoopBoundary else HoldBoundary
          SpanResult _ next@(Cursor nextFrame _) <- spanMixer pcm boundary from cursor
          let ended = nextFrame >= pcmFrames pcm
          commitCursor voice revision (if ended then Cursor (pcmFrames pcm) 0 else next) ended
          pure 0
        Stream queue -> do
          SpanResult mixedThrough next@(Cursor nextFrame _) <-
            mixStream spanMixer queue from cursor (queueChunks queue)
          let ended = queueEnd queue == EndOfStream && nextFrame >= queueWrittenThrough queue
              finalCursor = if ended then Cursor (queueWrittenThrough queue) 0 else next
          commitCursor voice revision finalCursor ended
          pure (if ended then 0 else fromIntegral (endExclusive - mixedThrough))
        Retired -> pure 0

commitCursor :: Voice -> Word64 -> Cursor -> Bool -> IO ()
commitCursor voice@(Voice _ _ _ _ ref) revision next@(Cursor frame _) ended = do
  remove <- atomicModifyIORef' ref $ \current ->
    let advanced = current {playbackCursor = next}
    in if playbackRevision current /= revision
      then (current, False)
      else case playbackSource current of
        Retired -> (current, False)
        _ | ended -> (retired VoiceEnded advanced, True)
        Clip _ _ -> (advanced, False)
        Stream queue ->
          let consumed = min frame (queueWrittenThrough queue)
              pruned = queue
                { queueConsumedThrough = consumed
                , queueChunks = dropConsumed consumed (queueChunks queue)
                }
          in (advanced {playbackSource = Stream pruned}, False)
  when remove (unregisterVoice voice)

dropConsumed :: Int -> Seq Chunk -> Seq Chunk
dropConsumed frame chunks = case Seq.viewl chunks of
  Chunk start pcm :< rest | start + pcmFrames pcm <= frame -> dropConsumed frame rest
  _ -> chunks

data Boundary = LoopBoundary | HoldBoundary | OpenBoundary | NextChunk !Float !Float
  deriving (Eq)

data SpanResult = SpanResult !Int !Cursor

mixStream
  :: (PCM -> Boundary -> Int -> Cursor -> IO SpanResult)
  -> StreamQueue -> Int -> Cursor -> Seq Chunk -> IO SpanResult
mixStream spanMixer queue = go
  where
    go !outputFrame cursor@(Cursor frame fraction) chunks = case Seq.viewl (dropConsumed frame chunks) of
      EmptyL -> pure (SpanResult outputFrame cursor)
      Chunk start pcm :< remaining -> do
        boundary <- case Seq.viewl remaining of
          EmptyL -> pure (if queueEnd queue == EndOfStream then HoldBoundary else OpenBoundary)
          Chunk _ nextPCM :< _ -> withPCMSamples nextPCM $ \pointer -> do
            (left, right) <- readStereo pointer (channelCount (pcmChannels nextPCM)) 0
            pure (NextChunk left right)
        SpanResult mixedThrough (Cursor nextInChunk nextFraction) <-
          spanMixer pcm boundary outputFrame (Cursor (frame - start) fraction)
        let next = Cursor (start + nextInChunk) nextFraction
        if nextInChunk >= pcmFrames pcm
          then go mixedThrough next remaining
          else pure (SpanResult mixedThrough next)

mixSpan :: Ptr Float -> Int -> Int -> Float -> Float -> Int
  -> PCM -> Boundary -> Int -> Cursor -> IO SpanResult
mixSpan output outputRate sourceRate leftGain rightGain endExclusive pcm boundary from (Cursor initialFrame initialFraction) =
  withPCMSamples pcm $ \input ->
    let channels = channelCount (pcmChannels pcm)
        frames = pcmFrames pcm
        lastFrame = frames - 1
        loop !outputFrame !frame !fraction
          | outputFrame >= endExclusive || frame >= frames =
              pure (SpanResult outputFrame (Cursor frame fraction))
          | frame == lastFrame && fraction /= 0 && boundary == OpenBoundary =
              pure (SpanResult outputFrame (Cursor frame fraction))
          | otherwise = do
              (left, right) <- readStereo input channels frame
              if fraction == 0
                then addStereo output outputFrame (left * leftGain) (right * rightGain)
                else do
                  (nextLeft, nextRight) <- if frame < lastFrame
                    then readStereo input channels (frame + 1)
                    else case boundary of
                      LoopBoundary -> readStereo input channels 0
                      NextChunk nextLeft nextRight -> pure (nextLeft, nextRight)
                      _ -> pure (left, right)
                  let phase = fromIntegral fraction / fromIntegral outputRate
                  addStereo output outputFrame
                    ((left + (nextLeft - left) * phase) * leftGain)
                    ((right + (nextRight - right) * phase) * rightGain)
              let (whole, remainder) = (fraction + sourceRate) `quotRem` outputRate
                  advanced = frame + whole
                  next = if advanced >= frames && boundary == LoopBoundary then advanced `rem` frames else advanced
              loop (outputFrame + 1) next remainder
    in loop from initialFrame initialFraction
{-# INLINE mixSpan #-}

readStereo :: Ptr Float -> Int -> Int -> IO (Float, Float)
readStereo pointer channels frame
  | channels == 1 = do
      sample <- peekElemOff pointer frame
      pure (sample, sample)
  | otherwise = do
      left <- peekElemOff pointer (frame * 2)
      right <- peekElemOff pointer (frame * 2 + 1)
      pure (left, right)
{-# INLINE readStereo #-}

addStereo :: Ptr Float -> Int -> Float -> Float -> IO ()
addStereo pointer frame left right = do
  let leftIndex = frame * 2
      rightIndex = leftIndex + 1
  currentLeft <- peekElemOff pointer leftIndex
  currentRight <- peekElemOff pointer rightIndex
  pokeElemOff pointer leftIndex (currentLeft + left)
  pokeElemOff pointer rightIndex (currentRight + right)
{-# INLINE addStereo #-}

saturateBlock :: Ptr Float -> Int -> IO ()
saturateBlock pointer samples = go 0
  where
    go !sample
      | sample >= samples = pure ()
      | otherwise = do
          value <- peekElemOff pointer sample
          pokeElemOff pointer sample (clipSample value)
          go (sample + 1)
