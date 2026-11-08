{-# LANGUAGE OverloadedStrings #-}

module GameIO.Audio.Codec.Mp3 (loadMp3, decodeMp3, loadMp3Source) where

import Control.Exception (throwIO)
import Control.Monad (forM_, unless, when)
import Data.Array.Unboxed qualified as U
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word (Word32)
import GameIO.Audio.Codec.Mp3DSP
import GameIO.Audio.Codec.Mp3Frames
import GameIO.Audio.Codec.Source
import GameIO.Audio.PCM
import GameIO.Audio.Types

loadMp3 :: FilePath -> IO PCM
loadMp3 path = BS.readFile path >>= decodeMp3

-- | Decode mono/stereo MPEG-1, MPEG-2 or MPEG-2.5 Layer III. Xing/Info
-- metadata carrying LAME-compatible delay and padding trims the PCM.
decodeMp3 :: BS.ByteString -> IO PCM
decodeMp3 bytes = describeMp3 bytes >>= decodeWholeSource

loadMp3Source :: FilePath -> IO PCMSource
loadMp3Source path = BS.readFile path >>= describeMp3

describeMp3 :: BS.ByteString -> IO PCMSource
describeMp3 bytes = do
  source <- either throwIO pure (skipInitialTags bytes >>= findSync >>= readFrames)
  case source of
    [] -> throwIO (InvalidMp3 "No Layer III frames")
    Frame header raw : more -> do
      let info = readGapless header raw
          (frames, history, delay, padding) = case info of
            Nothing -> (source, BS.empty, 0, 0)
            Just (before, after) -> (more, tailReservoir (BS.drop (mainOffset header) raw), before, after)
          untrimmed = sum [toInteger (frameSamples frameHeader) | Frame frameHeader _ <- frames]
          total = untrimmed - toInteger (delay + padding)
      unless (total > 0) (throwIO (InvalidMp3 "No samples remain after gapless trim"))
      when (total > toInteger (maxBound :: Int)) (throwIO (AudioDecodeTooLarge maxBound))
      let extent = fromInteger total
      pure $ sequentialSource (mp3Rate header) (mp3Channels header) extent
        (produceFrames header delay extent history frames)

skipInitialTags :: BS.ByteString -> Either AudioError BS.ByteString
skipInitialTags bytes
  | not ("ID3" `BS.isPrefixOf` bytes) = Right bytes
  | BS.length bytes < 10 = Left (InvalidMp3 "Truncated ID3 header")
  | otherwise = do
      let flags = BS.index bytes 5
          sizeBytes = BS.take 4 (BS.drop 6 bytes)
      when (BS.any (>= 128) sizeBytes) (Left (InvalidMp3 "Invalid ID3 tag size"))
      let bodySize = BS.foldl' (\count byte -> count * 128 + fromIntegral byte) 0 sizeBytes
          footer = if testBit flags 4 then 10 else 0
          size = 10 + bodySize + footer
      when (size > BS.length bytes) (Left (InvalidMp3 "Truncated ID3 tag"))
      skipInitialTags (BS.drop size bytes)

findSync :: BS.ByteString -> Either AudioError BS.ByteString
findSync = go (0 :: Int)
  where
    go !skipped bytes
      | BS.length bytes < 4 || skipped > 65536 = Left (InvalidMp3 "No synchronized MPEG frame")
      | otherwise = case readHeader bytes of
          Right header
            | let size = frameBytes header
            , size <= BS.length bytes
            , skipped == 0 || follows header (BS.drop size bytes) -> Right bytes
          Left issue@(UnsupportedMp3 _) | skipped == 0 -> Left issue
          _ -> go (skipped + 1) (BS.drop 1 bytes)
    follows header bytes = case readHeader bytes of
      Right next -> sameFormat header next
      Left _ -> validTrailer bytes

readFrames :: BS.ByteString -> Either AudioError [Frame]
readFrames = go Nothing []
  where
    go initial frames bytes
      | validTrailer bytes = Right (reverse frames)
      | otherwise = do
          header <- readHeader bytes
          unless (maybe True (`sameFormat` header) initial) $
            Left (UnsupportedMp3 "Sample rate or channel layout changes within the file")
          let size = frameBytes header
          when (size > BS.length bytes) (Left (InvalidMp3 "Truncated final MPEG frame"))
          let (frame, rest) = BS.splitAt size bytes
          go (Just header) (Frame header frame : frames) rest

validTrailer :: BS.ByteString -> Bool
validTrailer bytes =
  BS.null bytes ||
  ("TAG" `BS.isPrefixOf` bytes && BS.length bytes == 128) ||
  "APETAGEX" `BS.isPrefixOf` bytes ||
  (BS.length bytes < 4096 && BS.all (== 0) bytes)

sameFormat :: FrameHeader -> FrameHeader -> Bool
sameFormat first next =
  mp3Rate first == mp3Rate next && mp3Channels first == mp3Channels next

-- Encoder delay/padding are packed as two 12-bit counts in the LAME tag.
-- The Layer III synthesis latency (529 samples) shifts both trim boundaries.
readGapless :: FrameHeader -> BS.ByteString -> Maybe (Int, Int)
readGapless header raw
  | BS.length body < 8 = Nothing
  | marker /= "Xing" && marker /= "Info" = Nothing
  | BS.length encoder < 24 = Just (0, 0)
  | not (any (`BS.isPrefixOf` encoder) ["LAME", "Lavc", "Lavf", "GOGO"]) = Just (0, 0)
  | otherwise =
      let byte index = fromIntegral (BS.index encoder index) :: Int
          delay = (byte 21 `shiftL` 4) .|. (byte 22 `shiftR` 4)
          padding = ((byte 22 .&. 15) `shiftL` 8) .|. byte 23
      in if delay == 0 && padding == 0 then Just (0, 0)
         else Just (delay + 529, max 0 (padding - 529))
  where
    body = BS.drop (mainOffset header) raw
    marker = BS.take 4 body
    flags = BS.foldl' (\word byte -> (word `shiftL` 8) .|. fromIntegral byte)
      (0 :: Word32) (BS.take 4 (BS.drop 4 body))
    sizes = zip [0 ..] [4, 4, 100, 4]
    offset = 8 + sum [size | (flag, size) <- sizes, testBit flags flag]
    encoder = BS.drop offset body

tailReservoir :: BS.ByteString -> BS.ByteString
tailReservoir bytes = BS.drop (max 0 (BS.length bytes - 511)) bytes

data Mp3Cursor = Mp3Cursor !Int !Int !BS.ByteString ![Frame]

produceFrames :: FrameHeader -> Int -> Int -> BS.ByteString -> [Frame] -> IO (IO (Maybe PCM))
produceFrames first delay extent initialHistory frames = do
  synth <- newSynth channels
  cursor <- newIORef (Mp3Cursor delay extent initialHistory frames)
  pure (nextFrame synth cursor)
  where
    channels = mp3Channels first
    rate = mp3Rate first
    lanes = channelCount channels
    nextFrame synth cursor = do
      Mp3Cursor skip remaining history pending <- readIORef cursor
      case pending of
        _ | remaining == 0 -> pure Nothing
        [] -> throwIO (InvalidMp3 "Decoded extent differs from MPEG frame count")
        frame@(Frame header _) : rest -> do
          (granules, nextHistory) <- either throwIO pure (readFrame history frame)
          samples <- traverse (synthesize synth header) granules
          let sampleCount = frameSamples header
              skipped = min skip sampleCount
              count = min remaining (sampleCount - skipped)
          writeIORef cursor (Mp3Cursor (skip - skipped) (remaining - count) nextHistory rest)
          if count == 0 then nextFrame synth cursor else do
            pcm <- newPCM rate channels count $ \output _ ->
              forM_ (zip [0, 576 ..] samples) $ \(granuleStart, valuesByChannel) -> do
                let begin = max 0 (skipped - granuleStart)
                    end = min 576 (skipped + count - granuleStart)
                forM_ (zip [0 ..] valuesByChannel) $ \(channel, values) ->
                  forM_ [begin .. end - 1] $ \index ->
                    writeCheckedSample output ((granuleStart + index - skipped) * lanes + channel)
                      (values U.! index)
            pure (Just pcm)
