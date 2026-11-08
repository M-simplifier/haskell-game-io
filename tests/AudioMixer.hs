{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket, throwIO, try)
import Control.Monad (forM_, unless)
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int16)
import Data.List qualified as List
import Data.Ratio ((%))
import Foreign.Marshal.Array (allocaArray, peekArray)
import GameIO.Audio.Mixer
import GameIO.Audio.PCM
import GameIO.Audio.Types
import GameIO.Audio.Wav

main :: IO ()
main = do
  schedulingLaw
  streamLaw
  transportLaw
  wavLaw
  putStrLn "Audio mixer, scheduling, streams and WAV checks passed"

schedulingLaw :: IO ()
schedulingLaw = do
  let loopSource = [(0.25, 0.5), (-0.75, 0.25), (0.125, -0.25)]
      noteSource = [(0.6, 0.6), (0.4, 0.4), (-0.5, -0.5), (0.1, 0.1)]
      voices =
        [ ReferenceVoice 0 127 44100 Loop 0.8 (-0.25) loopSource
        , ReferenceVoice 127 329 32000 Loop 0.9 0.5 noteSource
        , ReferenceVoice 127 480 96000 PlayOnce 0.3 0 [(1, 0), (-1, 0), (1, 0)]
        ]
      expected = referenceBlock 48000 480 voices
      shifted = referenceBlock 48000 480
        [ReferenceVoice 0 128 44100 Loop 0.8 (-0.25) loopSource, voices !! 1, voices !! 2]
  (full, final) <- renderSchedule [480]
  (split, splitFinal) <- renderSchedule [127, 353]
  (irregular, irregularFinal) <- renderSchedule [1, 93, 33, 11, 191, 150, 1]
  check "identical PCM at the scheduled on/off split" (full == split && split == irregular)
  check "identical final voice state at the scheduled split" (final == splitFinal && splitFinal == irregularFinal)
  check "independent rational time, gain and stereo balance" (near expected full)
  check "reference rejects a one-frame late stop" (not (near shifted full))
  where
    renderSchedule sizes = withMixer 48000 $ \mixer -> do
      loop <- pcmFromSamples 44100 Stereo [0.25, 0.5, -0.75, 0.25, 0.125, -0.25]
      note <- pcmFromSamples 32000 Mono [0.6, 0.4, -0.5, 0.1]
      oneShot <- pcmFromSamples 96000 Stereo [1, 0, -1, 0, 1, 0]
      looped <- schedule mixer 0 loop Loop 0.8 (-0.25)
      stopped <- schedule mixer 127 note Loop 0.9 0.5
      clipped <- schedule mixer 127 oneShot PlayOnce 0.3 0
      stopVoiceAt looped (AudioFrame 127)
      stopVoiceAt stopped (AudioFrame 329)
      samples <- concat <$> traverse (render mixer) sizes
      rejected <- try (playClipAt mixer (Just (AudioFrame 0)) loop defaultVoiceOptions)
      case rejected of
        Left (AudioFrameAlreadyReserved (AudioFrame 0) (AudioFrame 480)) -> pure ()
        _ -> fail "The mixer accepted an already reserved output frame"
      snapshots <- traverse readVoice [looped, stopped, clipped]
      pure (samples, snapshots)

data ReferenceVoice = ReferenceVoice
  !Int !Int !Int !LoopMode !Float !Float ![(Float, Float)]

-- Independent time calculation and interpolation. The production path
-- advances an integer remainder and writes a mutable stereo buffer.
referenceBlock :: Int -> Int -> [ReferenceVoice] -> [Float]
referenceBlock rate count voices = concatMap sample [0 .. count - 1]
  where
    sample frame =
      let sumVoice (accLeft, accRight) voice =
            let (l, r) = referenceVoice rate frame voice
            in (accLeft + l, accRight + r)
          (left, right) = List.foldl' sumVoice (0, 0) voices
          limit x | x > 1 = 1 | x < -1 = -1 | otherwise = x
      in [limit left, limit right]

referenceVoice :: Int -> Int -> ReferenceVoice -> (Float, Float)
referenceVoice rate output (ReferenceVoice start stop sourceRate mode gain pan samples)
  | output < start || output >= stop = (0, 0)
  | mode == PlayOnce && whole >= extent = (0, 0)
  | otherwise =
      let select index = samples !! (if mode == Loop then index `mod` extent else min (extent - 1) index)
          (left, right) = select whole
          (nextLeft, nextRight) = select (whole + 1)
          interpolate before after = (1 - phase) * before + phase * after
          balanceLeft = if pan > 0 then 1 - pan else 1
          balanceRight = if pan < 0 then 1 + pan else 1
      in (interpolate left nextLeft * gain * balanceLeft, interpolate right nextRight * gain * balanceRight)
  where
    position = toInteger ((output - start) * sourceRate) % toInteger rate
    whole = floor position
    phase = fromRational (position - fromIntegral whole)
    extent = length samples

streamLaw :: IO ()
streamLaw = do
  let source = [fromIntegral ((index * 19) `mod` 29 - 14) / 20 | index <- [0 .. 96 :: Int]]
      stereo = zip source source
      expected = referenceBlock 48000 111 [ReferenceVoice 0 111 44100 PlayOnce 1 0 stereo]
  contiguous <- mixChunks source [97] [111]
  split <- mixChunks source [1, 5, 30, 61] [7, 31, 73]
  check "resampling across stream PCM boundaries" (near expected contiguous && split == contiguous)
  withMixer 48000 $ \mixer -> do
    first <- pcmFromSamples 24000 Mono [0.5]
    second <- pcmFromSamples 24000 Mono [-0.5]
    stream <- openMixerStream mixer 24000 Mono 2
    let voice = streamVoice stream
    check "paused stream queues initial samples" . (== 1) =<< writeStream stream first
    check "paused stream stays silent" . (== [0, 0]) =<< render mixer 1
    resumeVoice voice
    (samples, report) <- renderReport mixer 2
    check "an open stream holds its fractional cursor without lookahead"
      (samples == [0.5, 0.5, 0, 0] && mixStarvedStreamFrames report == 1)
    check "new block resumes the same fractional cursor" . (== 1) =<< writeStream stream second
    finishStream stream
    check "linear interpolation crosses the newly arrived chunk" . (== [0, 0, -0.5, -0.5, -0.5, -0.5])
      =<< render mixer 3
    check "EOF preserves exact length" . (== VoiceSnapshot VoiceEnded 2 24000 1 0) =<< readVoice voice
  where
    mixChunks source blockSizes outputSizes = withMixer 48000 $ \mixer -> do
      stream <- openMixerStream mixer 44100 Mono 97
      let writeChunks _ [] = pure ()
          writeChunks remaining (size : sizes) = do
            let (chunk, later) = splitAt size remaining
            pcm <- pcmFromSamples 44100 Mono chunk
            check "full source block queued" . (== size) =<< writeStream stream pcm
            writeChunks later sizes
      writeChunks source blockSizes
      finishStream stream
      resumeVoice (streamVoice stream)
      concat <$> traverse (render mixer) outputSizes

transportLaw :: IO ()
transportLaw = withMixer 48000 $ \mixer -> do
  pcm <- pcmFromSamples 48000 Mono [0.75, 0.75, -0.25]
  voice <- playClip mixer pcm defaultVoiceOptions
  duplicate <- playClip mixer pcm defaultVoiceOptions
  check "mix saturation is after summing all voices" . (== [1, 1]) =<< render mixer 1
  stopVoice duplicate
  pauseVoice voice
  check "paused voice position is unchanged by output time" . (== [0, 0]) =<< render mixer 1
  check "pause preserves its source frame" . (== 1) . voiceSourceFrame =<< readVoice voice
  seekVoice voice 2
  setVoiceMix voice 0.5 1
  resumeVoice voice
  check "seek and balance take effect on resume" . (== [0, -0.125]) =<< render mixer 1
  check "voice retires at finite source end" . (== VoiceEnded) . voiceStatus =<< readVoice voice
  stream <- openMixerStream mixer 48000 Mono 3
  check "stream block fills capacity" . (== 3) =<< writeStream stream pcm
  check "bounded stream rejects a block atomically" . (== 0) =<< writeStream stream pcm
  resetStream stream
  check "reset releases queued frames" . (== 3) =<< streamWritableFrames stream
  resumeVoice (streamVoice stream)
  check "old queued data is absent after reset" . (== [0, 0]) =<< render mixer 1
  closeMixer mixer
  check "engine close retires a held stream" . (== VoiceClosed) . voiceStatus =<< readVoice (streamVoice stream)

wavLaw :: IO ()
wavLaw = do
  forM_ [(1, Mono, [minBound, 8192, maxBound]), (2, Stereo, [minBound, 8192, maxBound, -8192])] $
    \(channels, layout, source) -> do
      let bytes = wav16 channels source
      pcm <- decodeWav bytes
      check "WAV preserves sample rate and channel layout" (pcmSampleRate pcm == 44100 && pcmChannels pcm == layout)
      decoded <- withPCMSamples pcm (peekArray (length source))
      check "WAV signed PCM16 amplitude" (decoded == map ((/ 32768) . fromIntegral) source)
      truncated <- try (decodeWav (BS.take (BS.length bytes - 1) bytes))
      case truncated of
        Left (InvalidWav _) -> pure ()
        _ -> fail "Truncated WAV accepted"

wav16 :: Int -> [Int16] -> BS.ByteString
wav16 channels samples = BL.toStrict $ runPut $ do
  putByteString "RIFF"
  putWord32le (fromIntegral (BL.length body))
  putLazyByteString body
  where
    body = runPut $ do
      putByteString "WAVEJUNK"
      putWord32le 1
      putWord16le 42 -- Unknown odd-sized chunk and its pad byte.
      putByteString "fmt "
      putWord32le 16
      putWord16le 1
      putWord16le (fromIntegral channels)
      putWord32le 44100
      putWord32le (fromIntegral (44100 * channels * 2))
      putWord16le (fromIntegral (channels * 2))
      putWord16le 16
      putByteString "data"
      putWord32le (fromIntegral (length samples * 2))
      forM_ samples (putWord16le . fromIntegral)

withMixer :: Int -> (Mixer -> IO a) -> IO a
withMixer rate = bracket (newMixer rate 8) closeMixer

schedule :: Mixer -> Word -> PCM -> LoopMode -> Float -> Float -> IO Voice
schedule mixer frame pcm mode gain pan = do
  options <- either throwIO pure (voiceOptions mode gain pan)
  playClipAt mixer (Just (AudioFrame (fromIntegral frame))) pcm options

render :: Mixer -> Int -> IO [Float]
render mixer frames = fst <$> renderReport mixer frames

renderReport :: Mixer -> Int -> IO ([Float], MixReport)
renderReport mixer frames = allocaArray (frames * 2) $ \pointer -> do
  report <- renderMixer mixer pointer frames
  samples <- peekArray (frames * 2) pointer
  pure (samples, report)

near :: [Float] -> [Float] -> Bool
near first second = length first == length second && and (zipWith (\x y -> abs (x - y) < 1e-6) first second)

check :: String -> Bool -> IO ()
check detail passed = unless passed (fail detail)
