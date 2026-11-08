{-# LANGUAGE OverloadedStrings #-}

module GameIO.Audio.Wav
  ( loadWav
  , decodeWav
  , loadWavSource
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Binary.Get
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int16, Int32)
import Data.IORef
import Data.Word (Word16, Word32)
import GHC.Float (castWord32ToFloat)
import GameIO.Audio.Codec.Source (PCMSource, directSource)
import GameIO.Audio.PCM
import GameIO.Audio.Types

data WavFormat = WavFormat !Word32 !Word16 !Word32 !Word16 !Word16

data SampleEncoding = Unsigned8 | Signed16 | Signed24 | Signed32 | IeeeFloat32

data WavePayload = WavePayload !Int !Channels !SampleEncoding !Int !Int !BS.ByteString

loadWav :: FilePath -> IO PCM
loadWav path = BS.readFile path >>= decodeWav

-- | Decode a little-endian RIFF/WAVE file. Unknown RIFF chunks are skipped.
-- Supports mono/stereo integer PCM and Float32, including extensible headers.
decodeWav :: BS.ByteString -> IO PCM
decodeWav bytes = do
  payload@(WavePayload _ _ _ _ frames _) <- describeWave bytes
  decodeRange payload 0 frames

loadWavSource :: FilePath -> IO PCMSource
loadWavSource path = do
  payload@(WavePayload rate channels _ _ frames _) <- BS.readFile path >>= describeWave
  pure $ directSource rate channels frames $ \offset -> do
    position <- newIORef offset
    pure $ do
      start <- readIORef position
      if start >= frames then pure Nothing else do
        let count = min 4096 (frames - start)
        chunk <- decodeRange payload start count
        writeIORef position (start + count)
        pure (Just chunk)

describeWave :: BS.ByteString -> IO WavePayload
describeWave bytes = do
  (WavFormat tag channels rate alignment bits, samples) <-
    either throwIO pure (parseWav getWave bytes)
  (layout, encoding, sampleBytes) <- case (channels, tag, bits) of
    (1, _, _) -> describe Mono tag bits
    (2, _, _) -> describe Stereo tag bits
    _ -> unsupported tag channels bits
  let frameBytes = sampleBytes * channelCount layout
      (frames, partial) = BS.length samples `quotRem` frameBytes
  unless (fromIntegral alignment == frameBytes) (throwIO (InvalidWav "Incorrect block alignment"))
  unless (partial == 0) (throwIO IncompletePCMFrame)
  either throwIO pure (validateSampleRate (fromIntegral rate))
  when (frames <= 0) (throwIO (InvalidFrameCount frames))
  pure (WavePayload (fromIntegral rate) layout encoding sampleBytes frames samples)
  where
    describe layout tag bits = case (tag, bits) of
      (1, 8) -> pure (layout, Unsigned8, 1)
      (1, 16) -> pure (layout, Signed16, 2)
      (1, 24) -> pure (layout, Signed24, 3)
      (1, 32) -> pure (layout, Signed32, 4)
      (3, 32) -> pure (layout, IeeeFloat32, 4)
      _ -> unsupported tag (fromIntegral (channelCount layout)) bits

    unsupported :: Word32 -> Word16 -> Word16 -> IO a
    unsupported tag channels bits =
      throwIO (UnsupportedWavFormat tag (fromIntegral channels) (fromIntegral bits))

decodeRange :: WavePayload -> Int -> Int -> IO PCM
decodeRange (WavePayload rate layout encoding sampleBytes _ samples) start frames =
  generatePCM rate layout frames $ \sample ->
    decodeSample encoding samples ((start * channelCount layout + sample) * sampleBytes)

parseWav :: Get a -> BS.ByteString -> Either AudioError a
parseWav parser bytes = case runGetOrFail parser (BL.fromStrict bytes) of
  Left (_, _, issue) -> Left (InvalidWav issue)
  Right (_, _, parsed) -> Right parsed

getWave :: Get (WavFormat, BS.ByteString)
getWave = do
  requireTag "RIFF"
  bytes <- fromIntegral <$> getWord32le
  isolate bytes $ do
    requireTag "WAVE"
    getChunks Nothing Nothing
  where
    getChunks !format !samples = do
      done <- isEmpty
      if done
        then case (format, samples) of
          (Just waveFormat, Just waveSamples) -> pure (waveFormat, waveSamples)
          _ -> fail "Missing fmt or data chunk"
        else do
          tag <- getByteString 4
          bytes <- fromIntegral <$> getWord32le
          (nextFormat, nextSamples) <- case (tag, format, samples) of
            ("fmt ", Nothing, _) -> do
              waveFormat <- isolate bytes getFormat
              pure (Just waveFormat, samples)
            ("data", _, Nothing) -> do
              waveSamples <- getByteString bytes
              pure (format, Just waveSamples)
            ("fmt ", _, _) -> fail "Duplicate fmt chunk"
            ("data", _, _) -> fail "Duplicate data chunk"
            _ -> skip bytes >> pure (format, samples)
          when (odd bytes) (skip 1)
          getChunks nextFormat nextSamples

requireTag :: BS.ByteString -> Get ()
requireTag expected = do
  tag <- getByteString (BS.length expected)
  unless (tag == expected) (fail ("Expected " ++ show expected))

getFormat :: Get WavFormat
getFormat = do
  tag <- getWord16le
  channels <- getWord16le
  rate <- getWord32le
  skip 4 -- Average bytes/sec. Block alignment defines the data layout.
  alignment <- getWord16le
  bits <- getWord16le
  actualTag <- if tag == 0xfffe then getExtensibleTag bits else pure (fromIntegral tag)
  _ <- getRemainingLazyByteString
  pure (WavFormat actualTag channels rate alignment bits)

getExtensibleTag :: Word16 -> Get Word32
getExtensibleTag containerBits = do
  extra <- getWord16le
  when (extra < 22) (fail "Short extensible WAVE header")
  isolate (fromIntegral extra) $ do
    validBits <- getWord16le
    when (validBits == 0 || validBits > containerBits) (fail "Invalid sample precision")
    skip 4 -- Speaker mask. Mono and stereo have a fixed order here.
    formatCode <- getWord32le
    requireTag (BS.pack [0, 0, 0x10, 0, 0x80, 0, 0, 0xaa, 0, 0x38, 0x9b, 0x71])
    skip (fromIntegral extra - 22)
    pure formatCode

decodeSample :: SampleEncoding -> BS.ByteString -> Int -> Float
decodeSample encoding bytes offset = case encoding of
  Unsigned8 -> (fromIntegral (at 0) - 128) / 128
  Signed16 -> fromIntegral (fromIntegral word16 :: Int16) / 32768
  Signed24 -> fromIntegral ((fromIntegral (word24 `shiftL` 8) :: Int32) `shiftR` 8) / 8388608
  Signed32 -> fromIntegral (fromIntegral word32 :: Int32) / 2147483648
  IeeeFloat32 -> castWord32ToFloat word32
  where
    -- decodeWav has already validated the full-frame data length.
    at index = fromIntegral (BS.index bytes (offset + index)) :: Word32
    word16 = at 0 .|. (at 1 `shiftL` 8)
    word24 = word16 .|. (at 2 `shiftL` 16)
    word32 = word24 .|. (at 3 `shiftL` 24)
