module GameIO.Audio.Codec.VorbisDSP
  ( BlockPlan
  , makeBlockPlan
  , blockSize
  , decodeBlock
  , Block (..)
  , packetMode
  ) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Data.Array qualified as A
import Data.Array.ST (STUArray, freeze, newArray, readArray, writeArray)
import Data.Array.Unboxed qualified as U
import Data.Bits ((.&.), shiftL, shiftR, xor)
import Data.ByteString qualified as BS
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.Ord (comparing)
import GameIO.Audio.Codec.VorbisSetup
import GameIO.Audio.Types

type Spectrum s = STUArray s (Int, Int) Float

data BlockPlan = BlockPlan !Int !(U.UArray Int Double) !(U.UArray Int Double) !(U.UArray Int Float)

data Block = Block !Int !(A.Array Int (U.UArray Int Float))

blockSize :: BlockPlan -> Int
blockSize (BlockPlan size _ _ _) = size

makeBlockPlan :: Int -> BlockPlan
makeBlockPlan size = BlockPlan size cosine sine window
  where
    phase index = 2 * pi * fromIntegral index / fromIntegral (size * 4)
    cosine = U.listArray (0, 2 * size - 1) [cos (phase index) | index <- [0 .. 2 * size - 1]]
    sine = U.listArray (0, 2 * size - 1) [sin (phase index) | index <- [0 .. 2 * size - 1]]
    window = U.listArray (0, size - 1)
      [ let value = sin (pi * (fromIntegral index + 0.5) / fromIntegral size)
        in realToFrac (sin (pi / 2 * value * value) :: Double)
      | index <- [0 .. size - 1] ]

packetST :: ST s a -> Packet s a
packetST = lift . lift . lift

modeHeader :: VorbisSetup -> Packet s Mode
modeHeader (VorbisSetup modes) = do
  header <- readFlag
  when header (packetError (InvalidVorbis "Expected an audio packet"))
  let (_, lastMode) = A.bounds modes
  readBits (integerLog lastMode) >>= checkedIndex "audio mode" modes

packetMode :: VorbisSetup -> BS.ByteString -> Either AudioError Bool
packetMode setup bytes = runPacket bytes $ do
  Mode long _ <- modeHeader setup
  pure long

decodeBlock :: VorbisSetup -> Int -> BlockPlan -> BlockPlan -> BS.ByteString -> Either AudioError Block
decodeBlock setup channels short long bytes = runPacket bytes $ do
  Mode longBlock (Mapping coupling mux submaps) <- modeHeader setup
  previousLong <- if longBlock then readFlag else pure False
  followingLong <- if longBlock then readFlag else pure False
  let select isLong = if isLong then long else short
      current = select longBlock
      previous = select previousLong
      following = select followingLong
      size = blockSize current
      bins = size `quot` 2
      channelIndices = [0 .. channels - 1]
      routed = zip channelIndices mux
      floors = A.listArray (0, length submaps - 1) [floorPlan | Submap floorPlan _ <- submaps]
  decodedFloors <- traverse (\index -> checkedIndex "channel floor" floors index >>= decodeFloor) mux
  let nonzero = IntSet.fromList [index | (index, present) <- zip channelIndices decodedFloors, isJust present]
      activate active (mag, ang)
        | IntSet.member mag active || IntSet.member ang active = IntSet.insert mag (IntSet.insert ang active)
        | otherwise = active
      residueChannels = List.foldl' activate nonzero coupling
  spectrum <- packetST (newSpectrum channels bins)
  forM_ (zip [0 ..] submaps) $ \(index, Submap _ residue) ->
    decodeResidue spectrum bins residueChannels [ch | (ch, submap) <- routed, submap == index] residue
  packetST $ forM_ (reverse coupling) (inverseCoupling spectrum bins)
  transformed <- forM (zip channelIndices decodedFloors) $ \(channel, floorCurve) ->
    packetST $ case floorCurve of
      Nothing -> pure (U.listArray (0, size - 1) (replicate size 0))
      Just points -> do
        applyFloor spectrum bins channel points
        transform current previous following spectrum channel
  pure (Block size (A.listArray (0, channels - 1) transformed))

newSpectrum :: Int -> Int -> ST s (Spectrum s)
newSpectrum channels bins = newArray ((0, 0), (channels - 1, bins - 1)) 0

decodeFloor :: Floor -> Packet s (Maybe [(Int, Int)])
decodeFloor (Floor1 multiplier xs classes) = do
  used <- readFlag
  if not used then pure Nothing else do
    let range = case multiplier of
          1 -> 256
          2 -> 128
          3 -> 86
          _ -> 64
        width = integerLog (range - 1)
    a <- readBits width
    b <- readBits width
    checkY range a
    checkY range b
    symbols <- concat <$> traverse readClass classes
    let (_, lastIndex) = U.bounds xs
        points = U.listArray (0, lastIndex) (a : b : symbols) :: U.UArray Int Int
        sortByX = List.sortOn (xs U.!) [0 .. lastIndex]
    values <- packetST (newInts (0, lastIndex))
    enabled <- packetST (newFlags (0, lastIndex))
    packetST $ do
      writeArray values 0 a
      writeArray values 1 b
      writeArray enabled 0 True
      writeArray enabled 1 True
    forM_ [2 .. lastIndex] $ \index -> do
      let x = xs U.! index
          previous = [0 .. index - 1]
          low = List.maximumBy (comparing (xs U.!)) (filter ((< x) . (xs U.!)) previous)
          high = List.minimumBy (comparing (xs U.!)) (filter ((> x) . (xs U.!)) previous)
          symbol = points U.! index
      lowY <- packetST (readArray values low)
      highY <- packetST (readArray values high)
      let predicted = lineAt (xs U.! low) lowY (xs U.! high) highY x
          highRoom = range - predicted
          room = 2 * min highRoom predicted
          restored
            | symbol == 0 = predicted
            | symbol >= room && highRoom > predicted = symbol
            | symbol >= room = predicted - symbol + highRoom - 1
            | odd symbol = predicted - (symbol + 1) `quot` 2
            | otherwise = predicted + symbol `quot` 2
      checkY range restored
      packetST $ do
        writeArray values index restored
        when (symbol /= 0) $ do
          writeArray enabled low True
          writeArray enabled high True
          writeArray enabled index True
    Just <$> packetST (gatherPoints xs multiplier enabled values sortByX)
  where
    checkY range value = unless (value >= 0 && value < range) $
      packetError (InvalidVorbis "Floor amplitude out of range")

readClass :: FloorClass -> Packet s [Int]
readClass (FloorClass count bits master children) = do
  code <- maybe (pure 0) decodeScalar master
  let mask = (1 `shiftL` bits) - 1
      go !remaining !selector
        | remaining <= 0 = pure []
        | otherwise = do
            book <- checkedIndex "floor subclass" children (selector .&. mask)
            value <- maybe (pure 0) decodeScalar book
            (value :) <$> go (remaining - 1) (selector `shiftR` bits)
  go count code

newInts :: (Int, Int) -> ST s (STUArray s Int Int)
newInts bounds = newArray bounds 0

newFlags :: (Int, Int) -> ST s (STUArray s Int Bool)
newFlags bounds = newArray bounds False

gatherPoints :: U.UArray Int Int -> Int -> STUArray s Int Bool -> STUArray s Int Int -> [Int] -> ST s [(Int, Int)]
gatherPoints xs multiplier enabled values = go
  where
    go [] = pure []
    go (index : rest) = do
      used <- readArray enabled index
      if not used then go rest else do
        value <- readArray values index
        ((xs U.! index, value * multiplier) :) <$> go rest

lineAt :: Int -> Int -> Int -> Int -> Int -> Int
lineAt left y right nextY x = y + ((nextY - y) * (x - left)) `quot` (right - left)

inverseDecibels :: U.UArray Int Float
inverseDecibels = U.listArray (0, 255)
  [realToFrac ((10 :: Double) ** (fromIntegral (index - 255) * 140 / (256 * 20))) | index <- [0 .. 255 :: Int]]

applyFloor :: Spectrum s -> Int -> Int -> [(Int, Int)] -> ST s ()
applyFloor spectrum bins channel = go
  where
    go ((from, startY) : next@(to, endY) : rest) = do
      forM_ [from .. min to bins - 1] $ \index ->
        apply index (lineAt from startY to endY index)
      when (to < bins) (go (next : rest))
    go [(from, value)] = forM_ [from .. bins - 1] (\index -> apply index value)
    go [] = pure ()
    apply index value = do
      sample <- readArray spectrum (channel, index)
      writeArray spectrum (channel, index) (sample * (inverseDecibels U.! value))

decodeResidue :: Spectrum s -> Int -> IntSet.IntSet -> [Int] -> Residue -> Packet s ()
decodeResidue spectrum bins enabled channels (Residue kind from to partitionSize classes classBook bookTable) = do
  let active = filter (`IntSet.member` enabled) channels
      interleaved = kind == 2
      decoders = if interleaved then [0 | not (null active)] else active
      width = if interleaved then bins * length channels else bins
      start = min width from
      extent = min width to - start
      partitions = extent `quot` partitionSize
      classWords = dimensions classBook
      route = U.listArray (0, length channels - 1) channels :: U.UArray Int Int
      add destination index value = do
        let address = if interleaved
              then let (frame, lane) = index `quotRem` length channels in (route U.! lane, frame)
              else (destination, index)
        old <- readArray spectrum address
        writeArray spectrum address (old + value)
  unless (null decoders || partitions <= 0) $ do
    assignments <- packetST (newAssignments (lastIndex decoders) (partitions + classWords))
    forM_ [0 .. 7] $ \pass ->
      forM_ [0, classWords .. partitions - 1] $ \group -> do
        when (pass == 0) $
          forM_ decoders $ \channel -> do
            word <- decodeScalar classBook
            packetST (assignWord assignments channel group classWords classes word)
        forM_ [group .. min (group + classWords) partitions - 1] $ \partition ->
          forM_ decoders $ \channel -> do
            classifier <- packetST (readArray assignments (channel, partition))
            case bookTable A.! (classifier, pass) of
              Nothing -> pure ()
              Just book -> do
                let offset = start + partition * partitionSize
                    dim = dimensions book
                if kind == 0
                  then do
                    let stride = partitionSize `quot` dim
                    forM_ [0 .. stride - 1] $ \index -> do
                      values <- decodeVector book
                      packetST $ forM_ (zip [0 ..] values) $ \(digit, value) ->
                        add channel (offset + index + digit * stride) value
                  else forM_ [0, dim .. partitionSize - 1] $ \index -> do
                    values <- decodeVector book
                    packetST $ forM_ (zip [index .. partitionSize - 1] values) $ \(sample, value) ->
                      add channel (offset + sample) value
  where
    lastIndex = List.foldl' max 0

newAssignments :: Int -> Int -> ST s (STUArray s (Int, Int) Int)
newAssignments channel rows = newArray ((0, 0), (channel, rows - 1)) 0

assignWord :: STUArray s (Int, Int) Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
assignWord assignments channel group dimension classes = go (dimension - 1)
  where
    go !index !word
      | index < 0 = pure ()
      | otherwise = do
          let (next, digit) = word `quotRem` classes
          writeArray assignments (channel, group + index) digit
          go (index - 1) next

inverseCoupling :: Spectrum s -> Int -> (Int, Int) -> ST s ()
inverseCoupling spectrum bins (magnitude, angle) = forM_ [0 .. bins - 1] $ \index -> do
  m <- readArray spectrum (magnitude, index)
  a <- readArray spectrum (angle, index)
  let (left, right)
        | m > 0 && a > 0 = (m, m - a)
        | m > 0 = (m + a, m)
        | a > 0 = (m, m + a)
        | otherwise = (m - a, m)
  writeArray spectrum (magnitude, index) left
  writeArray spectrum (angle, index) right

-- Reduce the IMDCT to a zero-padded DCT-IV and use a radix-2 inverse-sign
-- FFT. The Vorbis forward transform carries the normalization.
transform :: BlockPlan -> BlockPlan -> BlockPlan -> Spectrum s -> Int -> ST s (U.UArray Int Float)
transform current@(BlockPlan size cosine sine _) previous following spectrum channel = do
  real <- newDoubles size
  imaginary <- newDoubles size
  let half = size `quot` 2
  forM_ [0 .. half - 1] $ \index -> do
    sample <- realToFrac <$> readArray spectrum (channel, index)
    writeArray real index (sample * (cosine U.! (2 * index)))
    writeArray imaginary index (sample * (sine U.! (2 * index)))
  fft size cosine sine real imaginary
  output <- newSamples size
  forM_ [0 .. size - 1] $ \index -> do
    let phaseIndex = index + half `quot` 2
        (dctIndex, sign)
          | phaseIndex < half = (phaseIndex, 1)
          | phaseIndex < size = (size - phaseIndex - 1, -1)
          | otherwise = (phaseIndex - size, -1)
        phase = 2 * dctIndex + 1
    re <- readArray real dctIndex
    im <- readArray imaginary dctIndex
    let dct = re * (cosine U.! phase) - im * (sine U.! phase)
    writeArray output index (realToFrac (dct * sign) * windowValue current previous following index)
  freeze output

newDoubles :: Int -> ST s (STUArray s Int Double)
newDoubles size = newArray (0, size - 1) 0

newSamples :: Int -> ST s (STUArray s Int Float)
newSamples size = newArray (0, size - 1) 0

windowValue :: BlockPlan -> BlockPlan -> BlockPlan -> Int -> Float
windowValue (BlockPlan size _ _ _) (BlockPlan prior _ _ left) (BlockPlan next _ _ right) index
  | index < start || index >= rightEnd = 0
  | index < leftEnd = left U.! (index - start)
  | index < rightStart = 1
  | otherwise = right U.! (next `quot` 2 + index - rightStart)
  where
    start = size `quot` 4 - prior `quot` 4
    leftEnd = start + prior `quot` 2
    rightStart = 3 * size `quot` 4 - next `quot` 4
    rightEnd = rightStart + next `quot` 2

fft :: Int -> U.UArray Int Double -> U.UArray Int Double
  -> STUArray s Int Double -> STUArray s Int Double -> ST s ()
fft size cosine sine real imaginary = permute 1 0 >> stage 2
  where
    permute !index !previous
      | index >= size = pure ()
      | otherwise = do
          let reversed = nextReversed previous (size `quot` 2)
          when (index < reversed) $ do
            swap real index reversed
            swap imaginary index reversed
          permute (index + 1) reversed
    nextReversed !index !bit
      | bit == 0 = index
      | index .&. bit == 0 = index `xor` bit
      | otherwise = nextReversed (index `xor` bit) (bit `quot` 2)
    swap array a b = do
      x <- readArray array a
      y <- readArray array b
      writeArray array a y
      writeArray array b x
    stage !width
      | width > size = pure ()
      | otherwise = do
          let half = width `quot` 2
              stride = 4 * size `quot` width
          forM_ [0, width .. size - 1] $ \start ->
            forM_ [0 .. half - 1] $ \index -> do
              let low = start + index
                  high = low + half
                  angle = index * stride
                  c = cosine U.! angle
                  s = sine U.! angle
              lr <- readArray real low
              li <- readArray imaginary low
              hr <- readArray real high
              hi <- readArray imaginary high
              let re = hr * c - hi * s
                  im = hr * s + hi * c
              writeArray real low (lr + re)
              writeArray imaginary low (li + im)
              writeArray real high (lr - re)
              writeArray imaginary high (li - im)
          stage (width * 2)
