module GameIO.Audio.PCM
  ( PCM
  , pcmSampleRate
  , pcmChannels
  , pcmFrames
  , generatePCM
  , pcmFromSamples
  , slicePCM
  , withPCMSamples
  , readPCMSamples
  , newPCM
  , writeCheckedSample
  , clipSample
  ) where

import Control.Exception (throwIO)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, plusForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import System.IO (Handle, hGetBuf)
import GameIO.Audio.Types

data PCM = PCM !Int !Channels !Int !(ForeignPtr Float)

pcmSampleRate :: PCM -> Int
pcmSampleRate (PCM rate _ _ _) = rate

pcmChannels :: PCM -> Channels
pcmChannels (PCM _ channels _ _) = channels

pcmFrames :: PCM -> Int
pcmFrames (PCM _ _ frames _) = frames

-- | Allocate normalized, interleaved PCM. The callback index counts samples,
-- so stereo frame n occupies indices 2*n (left) and 2*n+1 (right).
-- Nonfinite values fail at the source. Finite values outside [-1,1] saturate.
generatePCM :: Int -> Channels -> Int -> (Int -> Float) -> IO PCM
generatePCM rate channels frames sampleAt =
  newPCM rate channels frames $ \pointer samples ->
    let writeSample !index
          | index >= samples = pure ()
          | otherwise = do
              let !value = sampleAt index
              writeCheckedSample pointer index value
              writeSample (index + 1)
    in writeSample 0

-- | Construct a short sound from a finite list of interleaved samples.
pcmFromSamples :: Int -> Channels -> [Float] -> IO PCM
pcmFromSamples rate channels values = do
  let (frames, partial) = length values `quotRem` channelCount channels
  if partial /= 0 then throwIO IncompletePCMFrame else
    newPCM rate channels frames $ \pointer _ ->
      let writeSamples !_ [] = pure ()
          writeSamples !index (value : remaining) = do
            writeCheckedSample pointer index value
            writeSamples (index + 1) remaining
      in writeSamples 0 values

-- | Take an immutable view in source frames. The slice keeps its source
-- allocation alive and can be queued without copying in the caller.
slicePCM :: PCM -> Int -> Int -> Either AudioError PCM
slicePCM (PCM rate channels extent samples) start count
  | start < 0 || start >= extent = Left (VoicePositionOutOfRange start)
  | count <= 0 || count > extent - start = Left (InvalidFrameCount count)
  | otherwise = Right (PCM rate channels count (plusForeignPtr samples (start * channelCount channels * 4)))

newPCM :: Int -> Channels -> Int -> (Ptr Float -> Int -> IO ()) -> IO PCM
newPCM rate channels frames initialize = do
  either throwIO pure (validateSampleRate rate)
  let channelsPerFrame = channelCount channels
  if frames <= 0 || frames > maxBound `quot` (4 * channelsPerFrame)
    then throwIO (InvalidFrameCount frames)
    else do
      let samples = frames * channelsPerFrame
      buffer <- mallocForeignPtrArray samples
      withForeignPtr buffer $ \pointer -> initialize pointer samples
      pure (PCM rate channels frames buffer)

writeCheckedSample :: Ptr Float -> Int -> Float -> IO ()
writeCheckedSample pointer index value
  | isNaN value || isInfinite value = throwIO (InvalidSample index)
  | otherwise = pokeElemOff pointer index (clipSample value)
{-# INLINE writeCheckedSample #-}

-- Internal: the mixer may read this pointer for the scope of the action.
withPCMSamples :: PCM -> (Ptr Float -> IO a) -> IO a
withPCMSamples (PCM _ _ _ buffer) = withForeignPtr buffer
{-# INLINE withPCMSamples #-}

-- Internal Float32 wire payload on Windows x64. The frame header and length
-- are checked before this buffer is allocated.
readPCMSamples :: Handle -> Int -> Channels -> Int -> IO PCM
readPCMSamples handle rate channels frames =
  newPCM rate channels frames $ \pointer samples -> do
    let bytes = samples * 4
    readBytes <- hGetBuf handle pointer bytes
    if readBytes /= bytes
      then throwIO (AudioTransportFailure "Truncated PCM payload")
      else
        let checkSample !index
              | index >= samples = pure ()
              | otherwise = do
                  value <- peekElemOff pointer index
                  if value >= -1 && value <= 1
                    then checkSample (index + 1)
                    else throwIO (InvalidSample index)
        in checkSample 0

clipSample :: Float -> Float
clipSample = max (-1) . min 1
{-# INLINE clipSample #-}
