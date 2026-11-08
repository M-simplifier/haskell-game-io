{-# LANGUAGE OverloadedStrings #-}

module GameIO.Audio.Codec.Vorbis (loadOggVorbis, decodeOggVorbis, loadOggVorbisSource) where

import Control.Exception (throwIO)
import Control.Monad (forM_, replicateM_, unless, when)
import Data.Array qualified as A
import Data.Array.Unboxed qualified as U
import Data.Binary.Get (Get, getByteString, getWord8, getWord32le, runGetOrFail, skip)
import Data.Bits ((.&.), shiftL, shiftR, testBit)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import GameIO.Audio.Codec.Ogg
import GameIO.Audio.Codec.Source
import GameIO.Audio.Codec.VorbisDSP
import GameIO.Audio.Codec.VorbisSetup
import GameIO.Audio.PCM
import GameIO.Audio.Types

data Identification = Identification !Int !Channels !Int !Int

data Link = Link !Int !Channels !BlockPlan !BlockPlan !VorbisSetup !Int !Int ![OggPacket]

loadOggVorbis :: FilePath -> IO PCM
loadOggVorbis path = BS.readFile path >>= decodeOggVorbis

-- | Decode Ogg Vorbis I floor-1 mono or stereo to normalized Float32 PCM.
-- Links in a chained file must use the same sample rate and channel layout.
-- The Ogg granule positions trim overlap output to the source's frame extent.
decodeOggVorbis :: BS.ByteString -> IO PCM
decodeOggVorbis bytes = describeVorbis bytes >>= decodeWholeSource

loadOggVorbisSource :: FilePath -> IO PCMSource
loadOggVorbisSource path = BS.readFile path >>= describeVorbis

describeVorbis :: BS.ByteString -> IO PCMSource
describeVorbis bytes = do
  links <- either throwIO pure (readOgg bytes >>= traverse prepareLink)
  case links of
    [] -> throwIO (InvalidOgg "No logical audio stream")
    Link rate channels _ _ _ _ _ _ : _ -> do
      let matches (Link r c _ _ _ _ _ _) = r == rate && c == channels
          total = sum [toInteger count | Link _ _ _ _ _ _ count _ <- links]
      unless (all matches links) (throwIO (UnsupportedVorbis "Chained links change source format"))
      when (total > toInteger (maxBound :: Int)) (throwIO (AudioDecodeTooLarge maxBound))
      pure (sequentialSource rate channels (fromInteger total) (produceLinks rate channels links))

prepareLink :: [OggPacket] -> Either AudioError Link
prepareLink packets = case packets of
  OggPacket identification _ _ : OggPacket comments _ _ : OggPacket setupPacket _ _ : audio -> do
    Identification rate channels shortSize longSize <- parseHeader getIdentification identification
    parseHeader getComments comments
    setupBody <- case BS.stripPrefix (BS.cons 5 "vorbis") setupPacket of
      Just body -> Right body
      Nothing -> Left (InvalidVorbis "Missing Vorbis setup header")
    setup <- readSetup (channelCount channels) setupBody
    (trimStart, frames) <- calculateExtent setup shortSize longSize audio
    pure (Link rate channels (makeBlockPlan shortSize) (makeBlockPlan longSize) setup trimStart frames audio)
  _ -> Left (InvalidVorbis "Missing Vorbis headers")

parseHeader :: Get a -> BS.ByteString -> Either AudioError a
parseHeader parser bytes = case runGetOrFail parser (BL.fromStrict bytes) of
  Left (_, _, issue) -> Left (InvalidVorbis issue)
  Right (_, _, value) -> Right value

getIdentification :: Get Identification
getIdentification = do
  tag <- getByteString 7
  unless (tag == BS.cons 1 "vorbis") (fail "Expected a Vorbis identification header")
  version <- getWord32le
  unless (version == 0) (fail "Unsupported Vorbis version")
  channels <- getWord8 >>= \count -> case count of
    1 -> pure Mono
    2 -> pure Stereo
    _ -> fail "Only mono and stereo Vorbis are supported"
  rate <- fromIntegral <$> getWord32le
  unless (either (const False) (const True) (validateSampleRate rate)) (fail "Invalid Vorbis sample rate")
  skip 12 -- Nominal, upper and lower bitrates are advisory.
  sizeByte <- getWord8
  let short = fromIntegral (sizeByte .&. 0x0f)
      long = fromIntegral (sizeByte `shiftR` 4)
  unless (short >= 6 && long <= 13 && short <= long) (fail "Invalid Vorbis block sizes")
  frame <- getWord8
  unless (testBit frame 0) (fail "Invalid identification framing bit")
  pure (Identification rate channels (1 `shiftL` short) (1 `shiftL` long))

getComments :: Get ()
getComments = do
  tag <- getByteString 7
  unless (tag == BS.cons 3 "vorbis") (fail "Expected a Vorbis comment header")
  skipText
  entries <- getWord32le
  when (entries > 1048576) (fail "Vorbis comment count exceeds decoder limit")
  replicateM_ (fromIntegral entries) skipText
  framed <- getWord8
  unless (testBit framed 0) (fail "Missing comment framing bit")
  where
    skipText = getWord32le >>= skip . fromIntegral

calculateExtent :: VorbisSetup -> Int -> Int -> [OggPacket] -> Either AudioError (Int, Int)
calculateExtent setup short long = go Nothing 0 Nothing
  where
    go _ _ _ [] = Left (InvalidVorbis "Missing final audio granule")
    go previous !count startOffset (OggPacket bytes granule eos : rest) = do
      isLong <- packetMode setup bytes
      let size = if isLong then long else short
          nextCount = count + maybe 0 (\before -> toInteger ((before + size) `quot` 4)) previous
      case (granule, eos) of
        (Just final, True) -> do
          let offset = maybe 0 id startOffset
              skipFrames = max 0 (negate offset)
              frames = toInteger final - max 0 offset
          unless (null rest && frames > 0 && frames <= nextCount - skipFrames) $
            Left (InvalidVorbis "Final granule is outside the decoded PCM extent")
          when (frames > toInteger (maxBound :: Int) || skipFrames > toInteger (maxBound :: Int)) $
            Left (AudioDecodeTooLarge maxBound)
          pure (fromInteger skipFrames, fromInteger frames)
        (Just position, False) | nextCount > 0 && startOffset == Nothing ->
          go (Just size) nextCount (Just (toInteger position - nextCount)) rest
        _ -> go (Just size) nextCount startOffset rest

data LinkCursor = LinkCursor !BlockPlan !BlockPlan !VorbisSetup !(Maybe Block) !Int !Int ![OggPacket]

data VorbisCursor = VorbisCursor ![Link] !(Maybe LinkCursor)

produceLinks :: Int -> Channels -> [Link] -> IO (IO (Maybe PCM))
produceLinks rate channels links = do
  cursor <- newIORef (VorbisCursor links Nothing)
  pure (nextChunk cursor)
  where
    lanes = channelCount channels
    nextChunk cursor = do
      VorbisCursor nextLinks active <- readIORef cursor
      case active of
        Nothing -> startLink cursor nextLinks
        Just (LinkCursor _ _ _ _ _ 0 _) -> startLink cursor nextLinks
        Just (LinkCursor _ _ _ _ _ _ []) -> throwIO (InvalidVorbis "Incomplete PCM output")
        Just (LinkCursor short long setup previous prefix remaining (OggPacket bytes _ _ : rest)) -> do
          block@(Block size now) <- either throwIO pure (decodeBlock setup lanes short long bytes)
          let continue dropFrames frames =
                writeIORef cursor (VorbisCursor nextLinks
                  (Just (LinkCursor short long setup (Just block) dropFrames frames rest)))
          case previous of
            Nothing -> continue prefix remaining >> nextChunk cursor
            Just (Block previousSize earlier) -> do
              let overlap = (previousSize + size) `quot` 4
                  skipHere = min overlap prefix
                  count = min (overlap - skipHere) remaining
              continue (prefix - skipHere) (remaining - count)
              if count == 0 then nextChunk cursor else do
                pcm <- newPCM rate channels count $ \output _ ->
                  forM_ [0 .. count - 1] $ \index ->
                    forM_ [0 .. lanes - 1] $ \channel -> do
                      let inOverlap = skipHere + index
                          right = previousSize `quot` 2 + inOverlap
                          left = size `quot` 2 - overlap + inOverlap
                          old = if right < previousSize then earlier A.! channel U.! right else 0
                          new = if left >= 0 then now A.! channel U.! left else 0
                      writeCheckedSample output (index * lanes + channel) (old + new)
                pure (Just pcm)
    startLink _ [] = pure Nothing
    startLink cursor (Link _ _ short long setup trim frames packets : rest) = do
      writeIORef cursor (VorbisCursor rest (Just (LinkCursor short long setup Nothing trim frames packets)))
      nextChunk cursor
