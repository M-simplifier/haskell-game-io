module GameIO.Audio.Codec.Source
  ( PCMSource
  , PCMReader
  , sourceSampleRate
  , sourceChannels
  , sourceFrames
  , openPCMReader
  , readPCMBlock
  , directSource
  , sequentialSource
  , decodeWholeSource
  ) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar)
import Control.Exception (SomeException, mask, throwIO, try)
import Control.Monad (unless, when)
import Data.IORef
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import GameIO.Audio.PCM
import GameIO.Audio.Types

type Produce = IO (Maybe PCM)

data PCMSource = PCMSource !Int !Channels !Int !(Int -> IO Produce)

data PCMReader = PCMReader !Int !Channels !Produce !(MVar ReaderState)

data ReaderState = Reading !Int !(Maybe PCM) | Failed !SomeException

sourceSampleRate :: PCMSource -> Int
sourceSampleRate (PCMSource rate _ _ _) = rate

sourceChannels :: PCMSource -> Channels
sourceChannels (PCMSource _ channels _ _) = channels

sourceFrames :: PCMSource -> Int
sourceFrames (PCMSource _ _ frames _) = frames

-- Internal constructor for seekable immutable sample data.
directSource :: Int -> Channels -> Int -> (Int -> IO Produce) -> PCMSource
directSource = PCMSource

-- Internal constructor for a decoder whose overlap/reservoir begins at zero.
sequentialSource :: Int -> Channels -> Int -> IO Produce -> PCMSource
sequentialSource rate channels frames start =
  PCMSource rate channels frames $ \offset -> do
    next <- start
    remainder <- skip next offset
    pending <- newIORef remainder
    pure $ do
      first <- readIORef pending
      case first of
        Nothing -> next
        Just chunk -> writeIORef pending Nothing >> pure (Just chunk)
  where
    skip next count
      | count == 0 = pure Nothing
      | otherwise = do
          pcm <- next >>= maybe (throwIO IncompletePCMFrame) pure
          let extent = pcmFrames pcm
          if count < extent
            then Just <$> either throwIO pure (slicePCM pcm count (extent - count))
            else skip next (count - extent)

-- | Start an independent reader at a source-frame offset. Compressed
-- recordings replay the preceding codec state when seeking.
openPCMReader :: PCMSource -> Int -> IO PCMReader
openPCMReader (PCMSource rate channels frames create) offset = do
  unless (offset >= 0 && offset <= frames) (throwIO (VoicePositionOutOfRange offset))
  producer <- if offset == frames then pure (pure Nothing) else create offset
  PCMReader rate channels producer <$> newMVar (Reading (frames - offset) Nothing)

-- | Pull up to the requested number of source frames. Each successful call
-- advances this reader once. A failed or interrupted decode invalidates it.
readPCMBlock :: PCMReader -> Int -> IO (Maybe PCM)
readPCMBlock (PCMReader rate channels produce state) requested = mask $ \restore -> do
  when (requested <= 0) (throwIO (InvalidFrameCount requested))
  result <- modifyMVarMasked state $ \cursor -> case cursor of
    Failed issue -> pure (cursor, Left issue)
    Reading 0 _ -> pure (cursor, Right Nothing)
    Reading remaining pending -> do
      let count = min requested remaining
      decoded <- try (restore (readBlock count pending))
      case decoded of
        Left issue -> pure (Failed issue, Left issue)
        Right (pcm, leftover) -> pure (Reading (remaining - count) leftover, Right (Just pcm))
  either throwIO pure result
  where
    bytesPerFrame = 4 * channelCount channels
    readBlock count pending = do
      leftover <- newIORef pending
      pcm <- newPCM rate channels count $ \pointer _ ->
        let fill !offset candidate
              | offset == count = writeIORef leftover candidate
              | otherwise = do
                  chunk <- case candidate of
                    Just value -> pure value
                    Nothing -> produce >>= maybe (throwIO IncompletePCMFrame) pure
                  unless (pcmSampleRate chunk == rate && pcmChannels chunk == channels) $
                    throwIO StreamFormatMismatch
                  let extent = pcmFrames chunk
                      frames = min (count - offset) extent
                  withPCMSamples chunk $ \source ->
                    copyBytes (pointer `plusPtr` (offset * bytesPerFrame)) source (frames * bytesPerFrame)
                  next <- if frames == extent
                    then pure Nothing
                    else Just <$> either throwIO pure (slicePCM chunk frames (extent - frames))
                  fill (offset + frames) next
        in fill 0 pending
      rest <- readIORef leftover
      pure (pcm, rest)

decodeWholeSource :: PCMSource -> IO PCM
decodeWholeSource source = do
  let bytes = toInteger (sourceFrames source) * toInteger (4 * channelCount (sourceChannels source))
  when (bytes > 512 * 1024 * 1024) (throwIO (AudioDecodeTooLarge (sourceFrames source)))
  reader <- openPCMReader source 0
  readPCMBlock reader (sourceFrames source) >>= maybe (throwIO IncompletePCMFrame) pure
