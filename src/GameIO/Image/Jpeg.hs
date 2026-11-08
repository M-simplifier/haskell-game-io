module GameIO.Image.Jpeg (JpegError (..), decodeJpegRGBA) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import Data.Array qualified as A
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed ((!))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word32)
import GameIO.Image.Jpeg.Raster (renderPlane, renderRGBA)
import GameIO.Image.Jpeg.Syntax

type Decode s = ExceptT JpegError (ST s)
type ScanReader s = StateT Cursor (Decode s)
type Coefficients s = STUArray s Int Int16

data Cursor = Cursor !BS.ByteString !Int !Word32 !Int

data PlaneBuffer s = PlaneBuffer
  !Int !Int !(Coefficients s) !(STUArray s Int Int) !(STRef s (Maybe Quantization))

data ScanBuffer s = ScanBuffer
  !Component !Int !(Coefficients s) !(Maybe Huffman) !(Maybe Huffman)

decodeJpegRGBA :: BS.ByteString -> Either JpegError (Int, Int, BS.ByteString)
decodeJpegRGBA bytes = do
  JpegPlan frame adobe scans <- parseJpeg bytes
  planes <- runST $ runExceptT $ do
    buffers <- lift (traverse (newPlaneBuffer frame) (frameComponents frame))
    let byIndex = A.listArray (0, length buffers - 1) buffers
    forM_ scans (decodeScan frame byIndex)
    forM (zip (frameComponents frame) buffers) $
      \(component, PlaneBuffer columns _ coefficients progression quantization) -> do
        dc <- lift (readArray progression 0)
        unless (dc >= 0) (invalid "JPEG component has no DC scan")
        table <- lift (readSTRef quantization) >>= maybe (invalid "JPEG component has no quantization") pure
        lift (renderPlane frame component columns coefficients table)
  rgba <- renderRGBA frame adobe planes
  pure (frameWidth frame, frameHeight frame, rgba)

newPlaneBuffer :: Frame -> Component -> ST s (PlaneBuffer s)
newPlaneBuffer frame component = do
  let columns = mcuColumns frame * horizontal component
      rows = mcuRows frame * vertical component
  coefficients <- newArray (0, columns * rows * 64 - 1) 0
  progression <- newArray (0, 63) (-1)
  quantization <- newSTRef Nothing
  pure (PlaneBuffer columns rows coefficients progression quantization)

decodeScan :: Frame -> A.Array Int (PlaneBuffer s) -> Scan -> Decode s ()
decodeScan frame buffers scan = do
  work <- traverse bind (scanComponents scan)
  let (columns, rows) = case work of
        [ScanBuffer component _ _ _ _] ->
          (ceilDivide (frameWidth frame * horizontal component) (8 * maxHorizontal frame),
            ceilDivide (frameHeight frame * vertical component) (8 * maxVertical frame))
        _ -> (mcuColumns frame, mcuRows frame)
      total = columns * rows
      interval = if restartInterval scan == 0 then total else restartInterval scan
      packets !start !marker segments = case segments of
        [] -> unless (start == total) (invalid "Incomplete JPEG MCU stream")
        Entropy bytes restart : rest -> do
          let count = min interval (total - start)
              next = start + count
              expected = if next == total then Nothing else Just marker
          unless (count > 0 && restart == expected) (invalid "JPEG restart sequence or interval mismatch")
          predictors <- lift (newPredictors (length (frameComponents frame)))
          eob <- lift (newSTRef 0)
          let runMCUs = decodeMCUs frame scan work predictors eob columns start next
          (_, Cursor _ consumed _ leftBits) <- runStateT runMCUs (Cursor bytes 0 0 0)
          remainingEob <- lift (readSTRef eob)
          unless (remainingEob == 0 && consumed == BS.length bytes && leftBits < 8) $
            invalid "JPEG scan data exceeds its MCU extent"
          packets next ((marker + 1) .&. 7) rest
  packets 0 0 (scanEntropy scan)
  where
    bind (ScanComponent component table dc ac) = do
      let PlaneBuffer columns _ coefficients progression quantization =
            buffers A.! componentIndex component
          first = spectralFirst scan
          lastCoefficient = spectralLast scan
          high = previousBit scan
          low = currentBit scan
      existing <- lift (readSTRef quantization)
      case existing of
        Nothing -> lift (writeSTRef quantization (Just table))
        Just prior -> unless (prior == table) (invalid "JPEG quantization changed for a component")
      dcProgress <- lift (readArray progression 0)
      when (first > 0 && dcProgress < 0) (invalid "JPEG AC scan precedes its DC scan")
      forM_ [first .. lastCoefficient] $ \index -> do
        prior <- lift (readArray progression index)
        unless (if high == 0 then prior < 0 else prior == high) $
          invalid "Invalid JPEG successive approximation order"
        lift (writeArray progression index low)
      pure (ScanBuffer component columns coefficients dc ac)

newPredictors :: Int -> ST s (STUArray s Int Int)
newPredictors count = newArray (0, count - 1) 0

decodeMCUs
  :: Frame -> Scan -> [ScanBuffer s] -> STUArray s Int Int -> STRef s Int
  -> Int -> Int -> Int -> ScanReader s ()
decodeMCUs frame scan components dc eob columns from end = case components of
  [one@(ScanBuffer _ stride _ _ _)] ->
    let decodeOne mcu =
          let (y, x) = quotRem mcu columns
          in blockDecoder frame scan dc eob one (64 * (y * stride + x))
    in advance from decodeOne
  _ -> advance from $ \mcu -> do
    let (y, x) = quotRem mcu columns
    forM_ components $ \component@(ScanBuffer plan stride _ _ _) -> do
      let h = horizontal plan
          v = vertical plan
      forM_ [0 .. v - 1] $ \row ->
        forM_ [0 .. h - 1] $ \column ->
          let block = (y * v + row) * stride + x * h + column
          in blockDecoder frame scan dc eob component (64 * block)
  where
    advance !mcu action
      | mcu >= end = pure ()
      | otherwise = action mcu >> advance (mcu + 1) action

blockDecoder
  :: Frame -> Scan -> STUArray s Int Int -> STRef s Int -> ScanBuffer s -> Int -> ScanReader s ()
blockDecoder frame scan dc eob (ScanBuffer component _ coefficients dcBook acBook) block =
  case frameMode frame of
    Sequential -> do
      d <- needBook dcBook
      a <- needBook acBook
      decodeDC d dc (componentIndex component) coefficients block 0
      sequentialAC a coefficients block 1
    Progressive
      | spectralFirst scan == 0 && previousBit scan == 0 -> do
          d <- needBook dcBook
          decodeDC d dc (componentIndex component) coefficients block (currentBit scan)
      | spectralFirst scan == 0 -> do
          digit <- takeBits 1
          when (digit /= 0) $ do
            prior <- coefficient coefficients block 0
            putCoefficient coefficients block 0 (prior .|. shiftL 1 (currentBit scan))
      | otherwise -> do
          a <- needBook acBook
          let first = spectralFirst scan
              final = spectralLast scan
              approximation = currentBit scan
          if previousBit scan == 0
            then initialAC a eob coefficients block first final approximation
            else refineAC a eob coefficients block first final approximation
  where
    needBook = maybe (scanError "Missing JPEG scan codebook") pure
{-# INLINE blockDecoder #-}

decodeDC :: Huffman -> STUArray s Int Int -> Int -> Coefficients s -> Int -> Int -> ScanReader s ()
decodeDC book predictors channel coefficients block approximation = do
  width <- decodeHuffman book
  unless (width <= 11) (scanError "JPEG DC coefficient exceeds category 11")
  difference <- receiveSigned width
  previous <- st (readArray predictors channel)
  let next = previous + difference
  putCoefficient coefficients block 0 (shiftL next approximation)
  st (writeArray predictors channel next)

sequentialAC :: Huffman -> Coefficients s -> Int -> Int -> ScanReader s ()
sequentialAC book coefficients block = go
  where
    go !index
      | index > 63 = pure ()
      | otherwise = do
          code <- decodeHuffman book
          let run = shiftR code 4
              width = code .&. 15
              destination = index + run
          if width == 0 then case run of
            0 -> pure ()
            15 | index + 16 <= 64 -> go (index + 16)
            _ -> scanError "Invalid sequential JPEG zero run"
          else do
            unless (destination <= 63 && width <= 10) (scanError "JPEG AC run or category exceeds block")
            value <- receiveSigned width
            putCoefficient coefficients block destination value
            go (destination + 1)

-- T.81 Annex G. The EOB run counts this block when it is introduced.
initialAC :: Huffman -> STRef s Int -> Coefficients s -> Int -> Int -> Int -> Int -> ScanReader s ()
initialAC book eob coefficients block first final approximation = do
  run <- st (readSTRef eob)
  if run > 0 then st (writeSTRef eob (run - 1)) else go first
  where
    go !index
      | index > final = pure ()
      | otherwise = do
          code <- decodeHuffman book
          let run = shiftR code 4
              width = code .&. 15
              destination = index + run
          if width == 0
            then if run == 15
              then if index + 16 <= final + 1
                then go (index + 16)
                else scanError "Progressive JPEG zero run exceeds band"
              else endOfBandRun eob run
            else do
              unless (destination <= final && width <= 10) (scanError "Invalid progressive JPEG AC run")
              value <- receiveSigned width
              putCoefficient coefficients block destination (shiftL value approximation)
              go (destination + 1)

endOfBandRun :: STRef s Int -> Int -> ScanReader s ()
endOfBandRun eob width = do
  extra <- takeBits width
  st (writeSTRef eob (shiftL 1 width + extra - 1))

refineAC :: Huffman -> STRef s Int -> Coefficients s -> Int -> Int -> Int -> Int -> ScanReader s ()
refineAC book eob coefficients block first final approximation = do
  run <- st (readSTRef eob)
  if run > 0
    then refineRest first >> st (writeSTRef eob (run - 1))
    else nextCode first
  where
    step = shiftL 1 approximation
    nextCode !index
      | index > final = pure ()
      | otherwise = do
          code <- decodeHuffman book
          let zeros = shiftR code 4
              width = code .&. 15
          case width of
            0 | zeros == 15 -> advanceToZero index (15 :: Int) >>= nextCode . (+ 1)
            0 -> endOfBandRun eob zeros >> refineRest index
            1 -> do
              digit <- takeBits 1
              destination <- advanceToZero index zeros
              putCoefficient coefficients block destination (if digit == 0 then negate step else step)
              nextCode (destination + 1)
            _ -> scanError "Progressive JPEG refinement symbol exceeds one bit"

    advanceToZero !index !zeros
      | index > final = scanError "Progressive JPEG refinement run exceeds band"
      | otherwise = do
          prior <- coefficient coefficients block index
          if prior /= 0
            then refine index prior >> advanceToZero (index + 1) zeros
            else if zeros == 0 then pure index else advanceToZero (index + 1) (zeros - 1)

    refineRest !index
      | index > final = pure ()
      | otherwise = do
          prior <- coefficient coefficients block index
          when (prior /= 0) (refine index prior)
          refineRest (index + 1)

    refine index prior = do
      digit <- takeBits 1
      when (digit /= 0 && abs prior .&. step == 0) $
        putCoefficient coefficients block index (prior + if prior < 0 then negate step else step)

coefficient :: Coefficients s -> Int -> Int -> ScanReader s Int
coefficient coefficients block index = fromIntegral <$> st (readArray coefficients (block + index))
{-# INLINE coefficient #-}

putCoefficient :: Coefficients s -> Int -> Int -> Int -> ScanReader s ()
putCoefficient coefficients block index value = do
  unless (value >= -32768 && value <= 32767) (scanError "JPEG coefficient exceeds storage range")
  st (writeArray coefficients (block + index) (fromIntegral value))
{-# INLINE putCoefficient #-}

receiveSigned :: Int -> ScanReader s Int
receiveSigned 0 = pure 0
receiveSigned width = do
  value <- takeBits width
  let half = shiftL 1 (width - 1)
  pure (if value >= half then value else value + 1 - shiftL half 1)
{-# INLINE receiveSigned #-}

decodeHuffman :: Huffman -> ScanReader s Int
decodeHuffman (Huffman fast minima maxima offsets symbols) = do
  cursor <- get
  let next@(Cursor bytes offset bits available) = refill 8 cursor
  if available >= 8 then do
    let prefix = fromIntegral (shiftR bits (available - 8) .&. 255)
        code = fromIntegral (fast ! prefix) :: Int
        width = shiftR code 8
    if width > 0
      then put (Cursor bytes offset bits (available - width)) >> pure (code .&. 255)
      else put (Cursor bytes offset bits (available - 8)) >> readLong 9 prefix
  else put next >> readLong 1 0
  where
    readLong !width !previous
      | width > 16 = scanError "Invalid JPEG Huffman prefix"
      | otherwise = do
          digit <- takeBits 1
          let value = shiftL previous 1 .|. digit
              minimumCode = minima ! width
          if value >= minimumCode && value <= maxima ! width
            then pure (fromIntegral (symbols ! (offsets ! width + value - minimumCode)))
            else readLong (width + 1) value

takeBits :: Int -> ScanReader s Int
takeBits 0 = pure 0
takeBits width = do
  Cursor bytes offset bits available <- refill width <$> get
  if available < width then scanError "Truncated JPEG entropy code" else do
    let remaining = available - width
        value = shiftR bits remaining .&. (shiftL 1 width - 1)
    put (Cursor bytes offset bits remaining)
    pure (fromIntegral value)
{-# INLINE takeBits #-}

refill :: Int -> Cursor -> Cursor
refill width original@(Cursor bytes start reservoir count)
  | count >= width = original
  | otherwise = go start reservoir count
  where
    extent = BS.length bytes
    go !offset !bits !available
      | available >= width || offset >= extent = Cursor bytes offset bits available
      | otherwise =
          let value = BSU.unsafeIndex bytes offset
              next = offset + if value == 255 then 2 else 1
          in go next (shiftL bits 8 .|. fromIntegral value) (available + 8)
{-# INLINE refill #-}

st :: ST s a -> ScanReader s a
st = lift . lift
{-# INLINE st #-}

invalid :: String -> Decode s a
invalid = throwE . InvalidJPEG

scanError :: String -> ScanReader s a
scanError = lift . invalid
