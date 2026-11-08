module GameIO.Audio.Codec.Mp3DSP (Synth, newSynth, synthesize) where

import Control.Exception (throwIO)
import Control.Monad (forM_, replicateM, zipWithM)
import Data.Array.IO (IOUArray, freeze, newArray, readArray, thaw, writeArray)
import Data.Array.Unboxed qualified as U
import Data.Bits ((.&.))
import Data.IORef
import Data.List qualified as List
import GameIO.Audio.Codec.Mp3Frames
import GameIO.Audio.Codec.Mp3Tables (synthesisWindow)
import GameIO.Audio.Types

type Samples = U.UArray Int Float

data Lane = Lane !(IOUArray Int Float) !(IOUArray Int Float) !(IORef Int)

newtype Synth = Synth [Lane]

newSynth :: Channels -> IO Synth
newSynth channels = Synth <$> replicateM (channelCount channels)
  (Lane <$> newArray (0, 575) 0 <*> newArray (0, 1023) 0 <*> newIORef 0)

synthesize :: Synth -> FrameHeader -> [Quantized] -> IO [Samples]
synthesize (Synth lanes) header channels = do
  let decoded = map requantize channels
  joined <- case (channels, decoded) of
    ([left, right], [l, r]) | usesIntensity header || usesMidSide header ->
      joinStereo header left right l r
    _ -> pure decoded
  zipWithM decodeLane lanes (zip channels joined)

requantize :: Quantized -> Samples
requantize channel = U.listArray (0, 575) $ concatMap bandSamples (quantizedBands channel)
  where
    raw = quantizedSamples channel
    gain = 0.25 * fromIntegral (quantizedGain channel - 210)
    bandSamples (band, Scale scale _) =
      let start = bandOffset band
          logGain = gain - 0.5 * fromIntegral ((scale + pre) * quantizedScaleStep channel) - 2 * fromIntegral sub
          factor = 2 ** logGain
          pre = case bandWindow band of
            Nothing | quantizedPreemphasis channel -> preemphasis U.! bandIndex band
            _ -> 0
          sub = case bandWindow band of
            Just lane -> quantizedSubblockGain channel !! lane
            Nothing -> 0
      in [ let value = raw U.! index
               magnitude = pow43 U.! abs value
           in if value < 0 then negate (magnitude * factor) else magnitude * factor
         | index <- [start .. start + bandWidth band - 1] ]

preemphasis :: U.UArray Int Int
preemphasis = U.listArray (0, 21) [0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,3,3,3,2,0]

pow43 :: Samples
pow43 = U.listArray (0, 8206) [fromIntegral index ** (4 / 3) | index <- [0 .. 8206 :: Int]]

joinStereo :: FrameHeader -> Quantized -> Quantized -> Samples -> Samples -> IO [Samples]
joinStereo header left right leftSamples rightSamples
  | not (usesIntensity header) = pure (fromStereo (map normal [0 .. 575]))
  | not aligned = throwIO (UnsupportedMp3 "Intensity stereo uses different channel bands")
  | otherwise = pure (fromStereo (concatMap stereoBand rightBands))
  where
    rightBands = quantizedBands right
    aligned = map (bandShape . fst) (quantizedBands left) == map (bandShape . fst) rightBands
    bandShape band = (bandOffset band, bandWidth band, bandWindow band)
    paired = [band | (band, _) <- rightBands]
    fromStereo both =
      let (l, r) = unzip both
      in [U.listArray (0, 575) l, U.listArray (0, 575) r]
    midSide = usesMidSide header
    normal index =
      let l = leftSamples U.! index
          r = rightSamples U.! index
      in if midSide then ((l + r) * sqrtHalf, (l - r) * sqrtHalf) else (l, r)
    sqrtHalf = sqrt 0.5

    active band = any ((/= 0) . (rightSamples U.!)) (indices band)
    coupled = any ((== Nothing) . bandWindow) paired
    lastActive window = List.foldl' max (-1)
      [bandIndex band | band <- paired, active band, coupled || bandWindow band == window]
    topCount = if quantizedBlock right `elem` [ShortBlock, MixedBlock] then 3 else 1
    tailStart = length rightBands - topCount
    lastByLane = [(lane, lastActive lane) | lane <- [Nothing, Just 0, Just 1, Just 2]]
    intensityFor band position =
      let priorIndex = bandIndex band - topCount
          prior = snd <$> at priorIndex rightBands
          top = bandIndex band >= tailStart
          maxBand = maybe (-1) id (lookup (bandWindow band) lastByLane)
          usedPosition
            | top && maxBand >= priorIndex = Just (if mpegVersion header == Mpeg1 then 3 else 0)
            | top = prior >>= \(Scale _ value) -> value
            | otherwise = position
      in if bandIndex band > maxBand then usedPosition else Nothing
    stereoBand (band, Scale _ position) = case intensityFor band position of
      Nothing -> map normal (indices band)
      Just index ->
        let (l, r) = stereoRatios (mpegVersion header) (quantizedIntensityStep right) index
        in [(l * sample, r * sample) | bin <- indices band, let sample = leftSamples U.! bin]
    indices band = [bandOffset band .. bandOffset band + bandWidth band - 1]
    at index xs = case drop index xs of
      item : _ | index >= 0 -> Just item
      _ -> Nothing

stereoRatios :: MpegVersion -> Int -> Int -> (Float, Float)
stereoRatios Mpeg1 _ position
  | position >= 6 = (1, 0)
  | otherwise =
      let tangent = tan (fromIntegral position * pi / 12)
      in (tangent / (1 + tangent), 1 / (1 + tangent))
stereoRatios _ step position =
  let factor = 2 ** (negate (fromIntegral ((position + 1) `quot` 2 * (1 + step))) / 4)
  in if odd position then (factor, 1) else (1, factor)

decodeLane :: Lane -> (Quantized, Samples) -> IO Samples
decodeLane (Lane overlap history headIndex) (channel, spectral) = do
  let block = quantizedBlock channel
      ordered = if block `elem` [ShortBlock, MixedBlock] then reorder (quantizedBands channel) spectral else spectral
  bins <- thaw ordered
  antiAlias block bins
  hybrid block overlap bins
  synthesizeSubbands history headIndex bins

reorder :: [(Band, Scale)] -> Samples -> Samples
reorder bands spectral = U.array (0, 575) $ concatMap place bands
  where
    place (band, _) =
      let start = bandOffset band
          width = bandWidth band
          destination index = case bandWindow band of
            Nothing -> start + index
            Just lane -> start - width * lane + 3 * index + lane
      in [(destination index, spectral U.! (start + index)) | index <- [0 .. width - 1]]

antiAlias :: BlockKind -> IOUArray Int Float -> IO ()
antiAlias block bins = forM_ [1 .. lastSubband] $ \subband ->
  forM_ (zip [0 ..] aliasCoefficients) $ \(index, (cs, ca)) -> do
    let upper = 18 * subband + index
        lower = 18 * subband - 1 - index
    a <- readArray bins lower
    b <- readArray bins upper
    writeArray bins lower (a * cs - b * ca)
    writeArray bins upper (b * cs + a * ca)
  where
    lastSubband = case block of ShortBlock -> 0; MixedBlock -> 1; _ -> 31

aliasCoefficients :: [(Float, Float)]
aliasCoefficients =
  [(cosine, factor * cosine) |
    factor <- [-0.6, -0.535, -0.33, -0.185, -0.095, -0.041, -0.0142, -0.0037],
    let cosine = recip (sqrt (1 + factor * factor))]

hybrid :: BlockKind -> IOUArray Int Float -> IOUArray Int Float -> IO ()
hybrid block overlap bins = do
  buffer <- newArray (0, 35) 0 :: IO (IOUArray Int Float)
  forM_ [0 .. 31] $ \subband -> do
    let start = subband * 18
        shape = if block == MixedBlock then if subband < 2 then LongBlock else ShortBlock else block
    if shape == ShortBlock then do
      forM_ [0 .. 35] $ \index -> writeArray buffer index 0
      forM_ [0 .. 2] $ \window ->
        forM_ [0 .. 11] $ \index -> do
          let outputIndex = 6 + window * 6 + index
          value <- dot 6 $ \k ->
            (*(shortCos U.! (index, k))) <$> readArray bins (start + window + 3 * k)
          prior <- readArray buffer outputIndex
          writeArray buffer outputIndex (prior + value * shortWindow index)
    else forM_ [0 .. 35] $ \index -> do
      value <- dot 18 $ \k -> (*(longCos U.! (index, k))) <$> readArray bins (start + k)
      writeArray buffer index (value * windowAt shape index)
    forM_ [0 .. 17] $ \index -> do
      first <- readArray buffer index
      next <- readArray buffer (index + 18)
      previous <- readArray overlap (start + index)
      let sample = first + previous
          inverted = odd subband && odd index
      writeArray bins (start + index) (if inverted then negate sample else sample)
      writeArray overlap (start + index) next

longCos, shortCos :: U.UArray (Int, Int) Float
longCos = imdctCosines 18
shortCos = imdctCosines 6

imdctCosines :: Int -> U.UArray (Int, Int) Float
imdctCosines bands = U.listArray ((0, 0), (2 * bands - 1, bands - 1))
  [realToFrac (cos (pi / fromIntegral bands * (fromIntegral t + 0.5 + fromIntegral bands / 2) * (fromIntegral k + 0.5)) :: Double)
  | t <- [0 .. 2 * bands - 1], k <- [0 .. bands - 1]]

shortWindow :: Int -> Float
shortWindow index = shortWindowValues U.! index

shortWindowValues, longWindowValues :: Samples
shortWindowValues = sineWindow 12
longWindowValues = sineWindow 36

sineWindow :: Int -> Samples
sineWindow size = U.listArray (0, size - 1)
  [realToFrac (sin (pi * (fromIntegral index + 0.5) / fromIntegral size) :: Double) | index <- [0 .. size - 1]]

windowAt :: BlockKind -> Int -> Float
windowAt StartBlock index
  | index < 18 = longWindowValues U.! index
  | index < 24 = 1
  | index < 30 = shortWindow (index - 18)
  | otherwise = 0
windowAt StopBlock index
  | index < 6 = 0
  | index < 12 = shortWindow (index - 6)
  | index < 18 = 1
  | otherwise = longWindowValues U.! index
windowAt _ index = longWindowValues U.! index

synthesizeSubbands :: IOUArray Int Float -> IORef Int -> IOUArray Int Float -> IO Samples
synthesizeSubbands history headIndex bins = do
  output <- newArray (0, 575) 0 :: IO (IOUArray Int Float)
  start <- readIORef headIndex
  let slot !time !previous
        | time >= 18 = writeIORef headIndex previous
        | otherwise = do
            let origin = (previous - 64) .&. 1023
                historyAt index = (origin + index) .&. 1023
            forM_ [0 .. 63] $ \row -> do
              value <- dot 32 $ \column ->
                (*(polyphaseCos U.! (row, column))) <$> readArray bins (column * 18 + time)
              writeArray history (historyAt row) value
            forM_ [0 .. 31] $ \lane -> do
              value <- dot 16 $ \group -> do
                let row = group `quot` 2 * 128 + if odd group then 96 else 0
                (*(synthesisWindow U.! (group * 32 + lane))) <$> readArray history (historyAt (row + lane))
              writeArray output (time * 32 + lane) value
            slot (time + 1) origin
  slot 0 start
  freeze output

polyphaseCos :: U.UArray (Int, Int) Float
polyphaseCos = U.listArray ((0, 0), (63, 31))
  [realToFrac (cos ((16 + fromIntegral row) * (2 * fromIntegral column + 1) * pi / 64) :: Double)
  | row <- [0 .. 63 :: Int], column <- [0 .. 31 :: Int]]

dot :: Int -> (Int -> IO Float) -> IO Float
dot count term = go 0 0
  where
    go !index !total
      | index >= count = pure total
      | otherwise = term index >>= go (index + 1) . (total +)
{-# INLINE dot #-}
