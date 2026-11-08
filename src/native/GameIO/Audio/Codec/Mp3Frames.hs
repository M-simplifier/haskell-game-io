module GameIO.Audio.Codec.Mp3Frames
  ( FrameHeader (..)
  , MpegVersion (..)
  , Frame (..)
  , readHeader
  , frameSamples
  , mainOffset
  , BlockKind (..)
  , Band (..)
  , Scale (..)
  , Quantized (..)
  , readFrame
  ) where

import Control.Monad (replicateM, unless, when)
import Data.Array qualified as A
import Data.Array.Unboxed qualified as U
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Word (Word16)
import GameIO.Audio.Codec.Mp3Tables
import GameIO.Audio.Types

data MpegVersion = Mpeg1 | Mpeg2 | Mpeg25
  deriving (Eq, Show)

data FrameHeader = FrameHeader
  { mpegVersion :: !MpegVersion
  , mp3Rate :: !Int
  , mp3Channels :: !Channels
  , bandTable :: !Int
  , frameBytes :: !Int
  , sideOffset :: !Int
  , sideBytes :: !Int
  , usesMidSide :: !Bool
  , usesIntensity :: !Bool
  }

data Frame = Frame !FrameHeader !BS.ByteString

data BlockKind = LongBlock | StartBlock | ShortBlock | MixedBlock | StopBlock
  deriving (Eq, Show)

data Band = Band
  { bandOffset :: !Int
  , bandWidth :: !Int
  , bandWindow :: !(Maybe Int)
  , bandIndex :: !Int
  }

data Scale = Scale !Int !(Maybe Int)

data Quantized = Quantized
  { quantizedBlock :: !BlockKind
  , quantizedBands :: ![(Band, Scale)]
  , quantizedSamples :: !(U.UArray Int Int)
  , quantizedGain :: !Int
  , quantizedScaleStep :: !Int
  , quantizedSubblockGain :: ![Int]
  , quantizedPreemphasis :: !Bool
  , quantizedIntensityStep :: !Int
  }

data Part = Part
  { partBits :: !Int
  , bigPairs :: !Int
  , globalGain :: !Int
  , scaleCode :: !Int
  , blockKind :: !BlockKind
  , regionTables :: ![Int]
  , region0Bands :: !Int
  , region1Bands :: !Int
  , subblockGain :: ![Int]
  , hasPreemphasis :: !Bool
  , doubleScale :: !Bool
  , count1Table :: !Int
  }

frameSamples :: FrameHeader -> Int
frameSamples header = if mpegVersion header == Mpeg1 then 1152 else 576

mainOffset :: FrameHeader -> Int
mainOffset header = sideOffset header + sideBytes header

readHeader :: BS.ByteString -> Either AudioError FrameHeader
readHeader bytes = do
  when (BS.length bytes < 4) (Left (InvalidMp3 "Truncated frame header"))
  let octet index = fromIntegral (BS.index bytes index) :: Int
      format = octet 1
      rates = octet 2
      modes = octet 3
  unless (octet 0 == 255 && format .&. 0xe0 == 0xe0) (Left (InvalidMp3 "Frame sync missing"))
  version <- case (format `shiftR` 3) .&. 3 of
    0 -> Right Mpeg25
    2 -> Right Mpeg2
    3 -> Right Mpeg1
    _ -> Left (InvalidMp3 "Reserved MPEG version")
  unless (format .&. 6 == 2) (Left (UnsupportedMp3 "Only Layer III is supported"))
  let bitrateIndex = rates `shiftR` 4
      sampleRateIndex = (rates `shiftR` 2) .&. 3
      mode = modes `shiftR` 6
      channels = if mode == 3 then Mono else Stereo
      joint = mode == 1
      protected = not (testBit format 0)
      pad = if testBit rates 1 then 1 else 0
      mpeg1 = version == Mpeg1
      bitrates = if mpeg1 then bitrate1 else bitrateLsf
  when (bitrateIndex == 0) (Left (UnsupportedMp3 "Free-format bitrate"))
  when (bitrateIndex == 15 || sampleRateIndex == 3) (Left (InvalidMp3 "Invalid bitrate or sample rate"))
  let divisor = case version of Mpeg1 -> 1; Mpeg2 -> 2; Mpeg25 -> 4
      rate = baseRates U.! sampleRateIndex `quot` divisor
      table = case version of
        Mpeg1 -> 5 + sampleRateIndex
        Mpeg2 -> 2 + sampleRateIndex
        Mpeg25 -> max 0 (sampleRateIndex - 1)
      headerLength = if protected then 6 else 4
      sideLength = case (mpeg1, channels) of
        (True, Mono) -> 17
        (True, Stereo) -> 32
        (False, Mono) -> 9
        (False, Stereo) -> 17
      count = ((if mpeg1 then 144000 else 72000) * (bitrates U.! bitrateIndex)) `quot` rate + pad
  unless (count >= headerLength + sideLength) (Left (InvalidMp3 "Frame is shorter than its side information"))
  pure (FrameHeader version rate channels table count headerLength sideLength
    (joint && testBit modes 5) (joint && testBit modes 4))
  where
    baseRates, bitrate1, bitrateLsf :: U.UArray Int Int
    baseRates = U.listArray (0, 2) [44100, 48000, 32000]
    bitrate1 = U.listArray (1, 14) [32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320]
    bitrateLsf = U.listArray (1, 14) [8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160]

data BitCursor = BitCursor !BS.ByteString !Int !Int

data BitFailure = PartEnd | BadPart !String

newtype Bits a = Bits (BitCursor -> Either BitFailure (a, BitCursor))

instance Functor Bits where
  fmap f (Bits parse) = Bits $ \state -> do
    (value, next) <- parse state
    pure (f value, next)

instance Applicative Bits where
  pure value = Bits (\state -> Right (value, state))
  (<*>) = apBits

apBits :: Bits (a -> b) -> Bits a -> Bits b
apBits functions values = do
  function <- functions
  function <$> values

instance Monad Bits where
  Bits parse >>= next = Bits $ \state -> do
    (value, updated) <- parse state
    let Bits continue = next value
    continue updated

runBits :: BS.ByteString -> Bits a -> Either AudioError a
runBits bytes (Bits parse) = case parse (BitCursor bytes 0 (8 * BS.length bytes)) of
  Left PartEnd -> Left (InvalidMp3 "Truncated Layer III side or main data")
  Left (BadPart detail) -> Left (InvalidMp3 detail)
  Right (value, _) -> Right value

reject :: String -> Bits a
reject detail = Bits (const (Left (BadPart detail)))

takeBits :: Int -> Bits Int
takeBits width = Bits $ \(BitCursor bytes offset limit) ->
  let end = offset + width
      loop !at !value
        | at >= end = value
        | otherwise =
            let (index, bitIndex) = at `quotRem` 8
                digit = fromIntegral ((BS.index bytes index `shiftR` (7 - bitIndex)) .&. 1)
            in loop (at + 1) ((value `shiftL` 1) .|. digit)
  in if end > limit
    then Left PartEnd
    else let !value = loop offset 0 in Right (value, BitCursor bytes end limit)
{-# INLINE takeBits #-}

flag :: Bits Bool
flag = (/= 0) <$> takeBits 1
{-# INLINE flag #-}

withinPart :: Int -> Bits a -> Bits a
withinPart count (Bits action) = Bits $ \(BitCursor bytes start limit) ->
  let end = start + count
  in if end > limit then Left PartEnd else do
    (value, _) <- action (BitCursor bytes start end)
    pure (value, BitCursor bytes end limit)

optionalQuad :: Bits [Int] -> Bits (Maybe [Int])
optionalQuad (Bits action) = Bits $ \state@(BitCursor bytes offset end) ->
  if offset >= end then pure (Nothing, state) else case action state of
    Left PartEnd -> Right (Nothing, BitCursor bytes end end)
    Left issue -> Left issue
    Right (value, next) -> Right (Just value, next)

readFrame :: BS.ByteString -> Frame -> Either AudioError ([[Quantized]], BS.ByteString)
readFrame reservoir (Frame header bytes) = do
  let side = BS.take (sideBytes header) (BS.drop (sideOffset header) bytes)
      body = BS.drop (mainOffset header) bytes
      main1 = mpegVersion header == Mpeg1
      channels = channelCount (mp3Channels header)
      privateBits = case (main1, channels) of
        (True, 1) -> 5
        (True, _) -> 3
        (False, 1) -> 1
        (False, _) -> 2
  (back, selectors, parts) <- runBits side $ do
    back <- takeBits (if main1 then 9 else 8)
    _ <- takeBits privateBits
    selectors <- if main1 then replicateM channels (replicateM 4 flag) else pure (replicate channels [])
    parts <- replicateM (if main1 then 2 else 1) (replicateM channels (readPart main1))
    pure (back, selectors, parts)
  unless (back <= BS.length reservoir) (Left (InvalidMp3 "Frame references missing bit-reservoir bytes"))
  let logical = BS.drop (BS.length reservoir - back) reservoir <> body
      history = reservoir <> body
      nextReservoir = BS.drop (max 0 (BS.length history - 511)) history
  granules <- runBits logical (readGranules header selectors parts)
  pure (granules, nextReservoir)

readPart :: Bool -> Bits Part
readPart main1 = do
  extent <- takeBits 12
  pairs <- takeBits 9
  when (pairs > 288) (reject "Too many pair-coded spectral lines")
  gain <- takeBits 8
  scale <- takeBits (if main1 then 4 else 9)
  switched <- flag
  (block, tables, first, second, subGains) <- if switched
    then do
      shape <- takeBits 2
      mixed <- flag
      block <- case (shape, mixed) of
        (1, False) -> pure StartBlock
        (2, False) -> pure ShortBlock
        (2, True) -> pure MixedBlock
        (3, False) -> pure StopBlock
        _ -> reject "Invalid switched block type"
      tables <- replicateM 2 (takeBits 5)
      gains <- replicateM 3 (takeBits 3)
      pure (block, tables ++ [0], if block == ShortBlock then 9 else 8, 40, gains)
    else do
      tables <- replicateM 3 (takeBits 5)
      first <- (+ 1) <$> takeBits 4
      second <- (+ 1) <$> takeBits 3
      pure (LongBlock, tables, first, second, [])
  pre <- if main1 then flag else pure (scale >= 500)
  scaleStep <- flag
  quads <- flag
  pure (Part extent pairs gain scale block tables first second subGains pre scaleStep (if quads then 33 else 32))

readGranules :: FrameHeader -> [[Bool]] -> [[Part]] -> Bits [[Quantized]]
readGranules header reuse = go (replicate (channelCount (mp3Channels header)) [])
  where
    go _ [] = pure []
    go previous (parts : rest) = do
      let partInputs = List.zip4 [0 ..] reuse previous parts
      current <- traverse readChannel partInputs
      later <- go (map (map snd . quantizedBands) current) rest
      pure (current : later)

    readChannel (channel, selectors, previous, part) = withinPart (partBits part) $ do
      when (mp3Rate header == 8000 && blockKind part == MixedBlock) $
        reject "8 kHz mixed Layer III blocks are unsupported"
      let plan = spectralBands header (blockKind part)
      scale <- readScales header channel selectors previous part plan
      values <- readSpectrum part plan
      pure (Quantized (blockKind part) (zip plan scale) values (globalGain part)
        (if doubleScale part then 2 else 1) (subblockGain part) (hasPreemphasis part) (scaleCode part .&. 1))

spectralBands :: FrameHeader -> BlockKind -> [Band]
spectralBands header block =
  let index = bandTable header
      widths = case block of
        ShortBlock -> shortWidths A.! index
        MixedBlock -> mixedWidths A.! index
        _ -> longWidths A.! index
      longCount = case block of
        ShortBlock -> 0
        MixedBlock -> if mpegVersion header == Mpeg1 then 8 else 6
        _ -> length widths
      windows = replicate longCount Nothing ++ cycle (map Just [0, 1, 2])
  in [Band offset width window band | (band, (offset, (width, window))) <-
        zip [0 ..] (zip (scanl (+) 0 widths) (zip widths windows))]

readScales :: FrameHeader -> Int -> [Bool] -> [Scale] -> Part -> [Band] -> Bits [Scale]
readScales header channel selectors previous part plan = do
  scales <- if mpegVersion header == Mpeg1
    then readMpeg1Scales
    else concat <$> traverse (\(width, count) -> replicateM count (valueAt width))
      (zip (lsfWidths intensity (scaleCode part)) (lsfCounts intensity block (scaleCode part)))
  let remaining = length plan - length scales
  unless (remaining == (if short then 3 else 1)) (reject "Invalid scale-factor count")
  pure (scales ++ replicate remaining (Scale 0 Nothing))
  where
    block = blockKind part
    short = block == ShortBlock || block == MixedBlock
    intensity = channel == 1 && usesIntensity header
    lsf = mpegVersion header /= Mpeg1
    valueAt width = do
      value <- takeBits width
      let invalidPosition = if lsf then width > 0 && value == (1 `shiftL` width) - 1 else value >= 7
      pure (Scale value (if invalidPosition then Nothing else Just value))

    readMpeg1Scales = do
      let (small, large) = mpeg1Widths A.! scaleCode part
      if short
        then let first = if block == MixedBlock then 17 else 18
             in (++) <$> replicateM first (valueAt small) <*> replicateM 18 (valueAt large)
        else concat <$> traverse readGroup
          (List.zip4 [0, 6, 11, 16] [6, 5, 5, 5] [small, small, large, large] (selectors ++ repeat False))
    readGroup (start, count, width, reuse)
      | reuse && not (null previous) = pure (take count (drop start previous))
      | otherwise = replicateM count (valueAt width)

mpeg1Widths :: A.Array Int (Int, Int)
mpeg1Widths = A.listArray (0, 15)
  [(0,0),(0,1),(0,2),(0,3),(3,0),(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(4,2),(4,3)]

lsfWidths :: Bool -> Int -> [Int]
lsfWidths intensity code
  | intensity && half < 180 = [half `quot` 36, (half `quot` 6) `rem` 6, half `rem` 6, 0]
  | intensity && half < 244 =
      let n = half - 180 in [n `quot` 16, (n `quot` 4) `rem` 4, n `rem` 4, 0]
  | intensity = let n = half - 244 in [n `quot` 3, n `rem` 3, 0, 0]
  | code < 400 = [code `quot` 80, (code `quot` 16) `rem` 5, (code `quot` 4) `rem` 4, code `rem` 4]
  | code < 500 = let n = code - 400 in [n `quot` 20, (n `quot` 4) `rem` 5, n `rem` 4, 0]
  | otherwise = let n = code - 500 in [n `quot` 3, n `rem` 3, 0, 0]
  where half = code `quot` 2

lsfCounts :: Bool -> BlockKind -> Int -> [Int]
lsfCounts intensity block code = table A.! (kind, group)
  where
    kind = case block of ShortBlock -> 1; MixedBlock -> 2; _ -> 0
    group
      | intensity && code < 360 = 3
      | intensity && code < 488 = 4
      | intensity = 5
      | code < 400 = 0
      | code < 500 = 1
      | otherwise = 2
    table :: A.Array (Int, Int) [Int]
    table = A.listArray ((0,0), (2,5))
      [ [6,5,5,5], [6,5,7,3], [11,10,0,0], [7,7,7,0], [6,6,6,3], [8,8,5,0]
      , [9,9,9,9], [9,9,12,6], [18,18,0,0], [12,12,12,0], [12,9,9,6], [15,12,9,0]
      , [6,9,9,9], [6,9,12,6], [15,18,0,0], [6,15,12,0], [6,12,9,6], [6,18,9,0]
      ]

readSpectrum :: Part -> [Band] -> Bits (U.UArray Int Int)
readSpectrum part bands = do
  let first = widthOf (region0Bands part)
      second = widthOf (region0Bands part + region1Bands part)
      table i
        | i < first = 0
        | i < second = 1
        | otherwise = 2
      bigEnd = 2 * bigPairs part
  coded <- concat <$> traverse (\index -> do
    let book = regionTables part !! table index
    (x, y) <- readPair book
    pure [x, y]) [0, 2 .. bigEnd - 1]
  small <- readQuads bigEnd
  pure (U.listArray (0, 575) (take 576 (coded ++ small ++ repeat 0)))
  where
    widthOf count = sum (map bandWidth (take count bands))
    readQuads index
      | index > 572 = pure []
      | otherwise = do
          values <- optionalQuad (readQuad (count1Table part))
          case values of
            Nothing -> pure []
            Just quad -> (quad ++) <$> readQuads (index + 4)

readPair :: Int -> Bits (Int, Int)
readPair 0 = pure (0, 0)
readPair number
  | number == 4 || number == 14 = reject "Reserved Layer III Huffman table"
  | otherwise = do
      let HuffmanBook linbits nodes = huffmanBooks A.! number
      pair <- huffmanValue nodes
      x <- sample linbits (pair `shiftR` 4)
      y <- sample linbits (pair .&. 15)
      pure (x, y)

readQuad :: Int -> Bits [Int]
readQuad number = do
  let HuffmanBook _ nodes = huffmanBooks A.! number
  quartet <- huffmanValue nodes
  traverse (\index -> sample 0 (if testBit quartet index then 1 else 0)) [3,2,1,0]

sample :: Int -> Int -> Bits Int
sample linbits magnitude
  | magnitude == 0 = pure 0
  | otherwise = do
      extension <- if magnitude == 15 then takeBits linbits else pure 0
      negative <- flag
      pure ((if negative then negate else id) (magnitude + extension))

huffmanValue :: U.UArray Int Word16 -> Bits Int
huffmanValue nodes = walk 0
  where
    node index
      | U.inRange (U.bounds nodes) index = pure (fromIntegral (nodes U.! index) :: Int)
      | otherwise = reject "Huffman code exceeds its table"
    walk !index = do
      value <- node index
      if value < 256 then pure value else do
        right <- flag
        branch right index value
    branch right index value =
      let offset = if right then value .&. 255 else value `shiftR` 8
          next = index + offset
      in if offset <= 0 then reject "Invalid Huffman branch"
         else if offset >= 250 then node next >>= branch right next
         else walk next
