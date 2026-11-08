{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Concurrent (forkIOWithUnmask, newEmptyMVar, putMVar, threadDelay, tryReadMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, unless, when)
import Data.Bits (testBit)
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekElemOff)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import GameIO.Audio
import GameIO.Audio.WASAPI (withLoopbackProbe)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= \case
  "--worker" : executable : args -> run (withIsolatedAudio executable) args
  args -> run withAudio args
  where
    run withDevice = \case
      [] -> verifyLoopback withDevice
      ["--stress", buffer, vertices]
        | Just bufferMs <- readMaybe buffer
        , Just liveVertices <- readMaybe vertices
        , liveVertices >= 0 && liveVertices <= 2000000 -> verifyLoad withDevice bufferMs liveVertices
      _ -> fail "Usage: audio-native [--worker WORKER_EXE] [--stress BUFFER_MS LIVE_VERTICES]"

type OpenDevice = AudioConfig -> (Audio -> IO ()) -> IO ()

verifyLoopback :: OpenDevice -> IO ()
verifyLoopback withDevice = withDevice defaultAudioConfig $ \audio -> do
  format <- readAudioFormat audio
  let rate = audioSampleRate format
      leftHz = 512
      rightHz = 864
  leftTone <- makeTone rate Mono leftHz 0.06
  rightTone <- makeTone rate Mono rightHz 0.04
  leftOptions <- either throwIO pure (voiceOptions Loop 1 (-1))
  rightOptions <- either throwIO pure (voiceOptions Loop 1 1)
  withLoopbackProbe rate $ \wait readPacket -> do
    left <- playSound audio leftTone leftOptions
    right <- playSound audio rightTone rightOptions
    drainFor wait readPacket 150000000
    (on, discontinuities) <- captureTones wait readPacket rate leftHz rightHz
    stopVoice left
    stopVoice right
    drainFor wait readPacket 100000000
    (off, _) <- captureTones wait readPacket rate leftHz rightHz
    print format
    print (audioWorkerPid audio)
    putStrLn ("Tone magnitudes (left at 512/864 Hz, right at 512/864 Hz): " ++ show on)
    putStrLn ("After stop: " ++ show off ++ ", capture discontinuities: " ++ show discontinuities)
    let (leftOwn, leftLeak, rightLeak, rightOwn) = on
        (leftStopped, _, _, rightStopped) = off
    unless (leftOwn > 0.015 && rightOwn > 0.01 &&
            leftLeak < leftOwn * 0.2 && rightLeak < rightOwn * 0.2 &&
            leftStopped < leftOwn * 0.1 && rightStopped < rightOwn * 0.1) $
      fail "The endpoint mix did not contain the expected panned tones and stop"
    print =<< readAudioStats audio

type ReadPacket = (Ptr Float -> Int -> Word32 -> IO ()) -> IO Bool

drainFor :: IO Bool -> ReadPacket -> Word64 -> IO ()
drainFor wait readPacket duration = do
  started <- getMonotonicTimeNSec
  let discard = do
        now <- getMonotonicTimeNSec
        when (now - started < duration) $ do
          readOne <- readPacket (\_ _ _ -> pure ())
          unless readOne (wait >> pure ())
          discard
  discard

captureTones :: IO Bool -> ReadPacket -> Int -> Double -> Double
  -> IO ((Double, Double, Double, Double), Int)
captureTones wait readPacket rate leftHz rightHz =
  captureFrames wait readPacket frames $ \output -> do
    leftOwn <- amplitude output 0 rate frames leftHz
    leftLeak <- amplitude output 0 rate frames rightHz
    rightLeak <- amplitude output 1 rate frames leftHz
    rightOwn <- amplitude output 1 rate frames rightHz
    pure (leftOwn, leftLeak, rightLeak, rightOwn)
  where
    frames = rate `quot` 2

captureFrames :: IO Bool -> ReadPacket -> Int -> (Ptr Float -> IO a) -> IO (a, Int)
captureFrames wait readPacket frames measure = do
  buffer <- mallocForeignPtrArray (frames * 2)
  withForeignPtr buffer $ \output -> do
    collected <- newIORef 0
    discontinuities <- newIORef 0
    started <- getMonotonicTimeNSec
    let consume input packetFrames flags = do
          offset <- readIORef collected
          let count = min packetFrames (frames - offset)
              target = output `plusPtr` (offset * 8)
          if testBit flags 1
            then fillBytes target 0 (count * 8)
            else copyBytes target input (count * 8)
          writeIORef collected (offset + count)
          when (testBit flags 0) (modifyIORef' discontinuities (+ 1))
        collect = do
          offset <- readIORef collected
          when (offset < frames) $ do
            now <- getMonotonicTimeNSec
            when (now - started > 2500000000) (fail "Timed out collecting WASAPI loopback packets")
            obtained <- readPacket consume
            unless obtained (wait >> pure ())
            collect
    collect
    observed <- measure output
    cuts <- readIORef discontinuities
    pure (observed, cuts)

amplitude :: Ptr Float -> Int -> Int -> Int -> Double -> IO Double
amplitude pointer channel rate frames frequency = go 0 0 0
  where
    radians = 2 * pi * frequency / fromIntegral rate
    go !frame !real !imag
      | frame >= frames = pure (2 * sqrt (real * real + imag * imag) / fromIntegral frames)
      | otherwise = do
          sample <- realToFrac <$> peekElemOff pointer (2 * frame + channel)
          let angle = fromIntegral frame * radians
          go (frame + 1) (real + sample * cos angle) (imag + sample * sin angle)

makeTone :: Int -> Channels -> Double -> Double -> IO PCM
makeTone rate channels frequency gain = generatePCM rate channels (rate `quot` 2) $ \sample ->
  let frame = if channels == Mono then sample else sample `quot` 2
  in realToFrac (gain * sin (2 * pi * frequency * fromIntegral frame / fromIntegral rate))

data Vertex = Vertex !Int !Int !Int !Int

buildVertices :: Int -> Int -> IntMap.IntMap Vertex
buildVertices seed count = IntMap.fromDistinctAscList
  [(index, Vertex index (index + seed) (index * 3) (seed - index)) | index <- [0 .. count - 1]]

vertexChecksum :: IntMap.IntMap Vertex -> Int
vertexChecksum = IntMap.foldl' (\checksum (Vertex x y z w) -> checksum + x + y + z + w) 0

verifyLoad :: OpenDevice -> Int -> Int -> IO ()
verifyLoad withDevice bufferMs liveVertices = do
  let rate = 48000
      leftHz = 600
      rightHz = 900
  config <- either throwIO pure (makeAudioConfig rate bufferMs 64)
  resampled <- makeTone 44100 Mono leftHz 0.0004
  nativeRate <- makeTone rate Stereo rightHz 0.0004
  leftOptions <- either throwIO pure (voiceOptions Loop 1 (-1))
  rightOptions <- either throwIO pure (voiceOptions Loop 1 1)
  live <- newIORef $! buildVertices 0 liveVertices
  withDevice config $ \audio -> do
    print =<< readAudioFormat audio
    print (audioWorkerPid audio)
    leftAsset <- prepareSound audio resampled
    rightAsset <- prepareSound audio nativeRate
    AudioFrame earliest <- readAudioFrame audio
    let start = AudioFrame (earliest + 24000)
    voices <- forM [0 .. 47 :: Int] $ \index ->
      if even index
        then playPreparedSoundAt leftAsset start leftOptions
        else playPreparedSoundAt rightAsset start rightOptions
    withLoopbackProbe rate $ \wait readPacket -> do
      drainFor wait readPacket 650000000
      before <- readAudioStats audio
      started <- getMonotonicTimeNSec
      completed <- newEmptyMVar
      _ <- forkIOWithUnmask $ \unmask ->
        try (unmask (exerciseHeap live 100 0 0)) >>= putMVar completed
      let capture = captureFrames wait readPacket (rate `quot` 2) (toneMinima rate leftHz rightHz)
          collect !windows !lowestLeft !lowestRight !captureCuts = do
            ((left, right), cuts) <- capture
            let observed = (windows + 50, min lowestLeft left, min lowestRight right, captureCuts + cuts)
            result <- tryReadMVar completed
            case result of
              Nothing -> let (n, l, r, c) = observed in collect n l r c
              Just (Left (issue :: SomeException)) -> throwIO issue
              Just (Right heap) -> pure (observed, heap)
      ((windows, lowestLeft, lowestRight, captureCuts), (checksum, longestGC)) <-
        collect (0 :: Int) 1 1 0
      finished <- getMonotonicTimeNSec
      after <- readAudioStats audio
      mapM_ stopVoice voices
      putStrLn ("100 forced major collections with " ++ show liveVertices ++
        " live vertices, checksum " ++ show checksum ++ ", max GC call ns " ++ show longestGC ++
        ", elapsed ns " ++ show (finished - started))
      putStrLn ("Before = " ++ show before)
      putStrLn ("After = " ++ show after)
      putStrLn ("10 ms endpoint windows " ++ show windows ++ ", minimum tone amplitudes " ++
        show (lowestLeft, lowestRight) ++ ", capture discontinuities " ++ show captureCuts)
      enabled <- getRTSStatsEnabled
      when enabled $ do
        stats <- getRTSStats
        putStrLn ("Last GC live bytes " ++ show (gcdetails_live_bytes (gc stats)) ++
          ", allocated bytes " ++ show (allocated_bytes stats))
      unless (audioStatus after == AudioRunning && submittedPackets after > submittedPackets before + 50)
        (fail "The audio worker stopped supplying the device under load")
      when (audioWorkerPid audio /= Nothing) $
        unless (lowestLeft > 0.0091 && lowestRight > 0.0091 && captureCuts == 0 &&
          emptyDeviceQueueEvents after == emptyDeviceQueueEvents before &&
          exceededPaddingDeadline after == exceededPaddingDeadline before)
          (fail "The isolated worker lost endpoint continuity under load")
  where
    exerciseHeap :: IORef (IntMap.IntMap Vertex) -> Int -> Int -> Word64 -> IO (Int, Word64)
    exerciseHeap live remaining !checksum !longest
      | remaining == 0 = do
          retained <- vertexChecksum <$> readIORef live
          pure (checksum + retained, longest)
      | otherwise = do
          let !renderFrame = vertexChecksum (buildVertices remaining 5000)
          threadDelay 12000
          started <- getMonotonicTimeNSec
          performMajorGC
          finished <- getMonotonicTimeNSec
          exerciseHeap live (remaining - 1) (checksum + renderFrame) (max longest (finished - started))

toneMinima :: Int -> Double -> Double -> Ptr Float -> IO (Double, Double)
toneMinima rate leftHz rightHz output = go 0 1 1
  where
    frames = rate `quot` 100
    go !window !lowestLeft !lowestRight
      | window == 50 = pure (lowestLeft, lowestRight)
      | otherwise = do
          let first = output `plusPtr` (window * frames * 8)
          left <- amplitude first 0 rate frames leftHz
          right <- amplitude first 1 rate frames rightHz
          go (window + 1) (min lowestLeft left) (min lowestRight right)
