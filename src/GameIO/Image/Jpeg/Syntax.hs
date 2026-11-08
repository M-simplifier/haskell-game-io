{-# LANGUAGE OverloadedStrings #-}

module GameIO.Image.Jpeg.Syntax
  ( JpegError (..), JpegPlan (..), Frame (..), FrameMode (..)
  , Component (..), Scan (..), ScanComponent (..), Entropy (..)
  , Huffman (..), Quantization, ceilDivide, parseJpeg
  ) where

import Control.Monad (foldM, unless)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (find)
import Data.Word (Word16, Word8)

data JpegError
  = InvalidJPEG !String
  | UnsupportedJPEG !String
  | JPEGDimensions !Int !Int
  deriving (Eq, Show)

data JpegPlan = JpegPlan !Frame !(Maybe Int) ![Scan]

data FrameMode = Sequential | Progressive
  deriving (Eq, Show)

data Frame = Frame
  { frameMode :: !FrameMode
  , frameWidth :: !Int
  , frameHeight :: !Int
  , maxHorizontal :: !Int
  , maxVertical :: !Int
  , mcuColumns :: !Int
  , mcuRows :: !Int
  , frameComponents :: ![Component]
  }

data Component = Component
  { componentIndex :: !Int
  , componentId :: !Int
  , horizontal :: !Int
  , vertical :: !Int
  , quantizationId :: !Int
  }

data Scan = Scan
  { scanComponents :: ![ScanComponent]
  , spectralFirst :: !Int
  , spectralLast :: !Int
  , previousBit :: !Int
  , currentBit :: !Int
  , restartInterval :: !Int
  , scanEntropy :: ![Entropy]
  }

data ScanComponent = ScanComponent !Component !Quantization !(Maybe Huffman) !(Maybe Huffman)
data Entropy = Entropy !BS.ByteString !(Maybe Int)
type Quantization = UArray Int Int

data Huffman = Huffman
  !(UArray Int Word16)  -- Lookahead holds code length and symbol.
  !(UArray Int Int)     -- First code at each length.
  !(UArray Int Int)     -- Last code at each length, -1 for a vacant length.
  !(UArray Int Int)     -- First symbol index at each length.
  !(UArray Int Word8)

data Tables = Tables
  { quantTables :: !(IntMap.IntMap Quantization)
  , codeTables :: !(IntMap.IntMap Huffman)
  , interval :: !Int
  , adobeTransform :: !(Maybe Int)
  }

-- ITU-T T.81 Annex B. A scan captures the table definitions in effect
-- at its SOS marker, including tables replaced between scans.
parseJpeg :: BS.ByteString -> Either JpegError JpegPlan
parseJpeg bytes = do
  require (BS.isPrefixOf "\xff\xd8" bytes) "JPEG SOI missing"
  collect 2 Nothing (Tables IntMap.empty IntMap.empty 0 Nothing) []
  where
    collect !offset frame tables scans = do
      (marker, afterMarker) <- markerAt bytes offset
      case marker of
        0xd9 -> case (frame, scans) of
          (Just image, _ : _) -> pure (JpegPlan image (adobeTransform tables) (reverse scans))
          _ -> Left (InvalidJPEG "JPEG has no frame or scan")
        0xd8 -> Left (InvalidJPEG "Unexpected JPEG SOI")
        0x01 -> collect afterMarker frame tables scans
        _ | marker >= 0xd0 && marker <= 0xd7 -> Left (InvalidJPEG "Restart marker outside a scan")
        _ -> do
          (body, next) <- segmentAt bytes afterMarker
          case marker of
            0xc0 -> frameHeader Sequential frame tables scans body next
            0xc1 -> frameHeader Sequential frame tables scans body next
            0xc2 -> frameHeader Progressive frame tables scans body next
            0xc4 -> do
              updated <- defineHuffman (codeTables tables) body
              collect next frame (tables {codeTables = updated}) scans
            0xdb -> do
              updated <- defineQuantization (quantTables tables) body
              collect next frame (tables {quantTables = updated}) scans
            0xdd -> do
              require (BS.length body == 2) "Invalid JPEG restart interval"
              collect next frame (tables {interval = word16 body 0}) scans
            0xda -> case frame of
              Nothing -> Left (InvalidJPEG "JPEG scan precedes its frame")
              Just image -> do
                (packets, following) <- entropyAt bytes next
                scan <- describeScan image tables body packets
                collect following frame tables (scan : scans)
            0xee
              | BS.length body >= 12 && BS.isPrefixOf "Adobe" body ->
                  collect next frame (tables {adobeTransform = Just (byte body 11)}) scans
            _ | elem marker [0xc3, 0xc5, 0xc6, 0xc7, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xdc, 0xde, 0xdf] ->
                  Left (UnsupportedJPEG "JPEG frame type, arithmetic coding or deferred dimensions")
              | otherwise -> collect next frame tables scans

    frameHeader mode frame tables scans body next = case frame of
      Just _ -> Left (UnsupportedJPEG "Multiple JPEG frames")
      Nothing -> do
        image <- describeFrame mode body
        collect next (Just image) tables scans

markerAt :: BS.ByteString -> Int -> Either JpegError (Int, Int)
markerAt bytes start = do
  require (start < BS.length bytes && byte bytes start == 255) "Expected JPEG marker"
  let next = start + 1 + BS.length (BS.takeWhile (== 255) (BS.drop (start + 1) bytes))
  require (next < BS.length bytes && byte bytes next /= 0) "Truncated or stuffed JPEG marker"
  pure (byte bytes next, next + 1)

segmentAt :: BS.ByteString -> Int -> Either JpegError (BS.ByteString, Int)
segmentAt bytes start = do
  require (start <= BS.length bytes - 2) "Truncated JPEG segment size"
  let lengthBytes = word16 bytes start
  require (lengthBytes >= 2 && start <= BS.length bytes - lengthBytes) "Invalid JPEG segment size"
  pure (BS.take (lengthBytes - 2) (BS.drop (start + 2) bytes), start + lengthBytes)

entropyAt :: BS.ByteString -> Int -> Either JpegError ([Entropy], Int)
entropyAt bytes start = go start start []
  where
    go !section !offset complete = case BS.elemIndex 255 (BS.drop offset bytes) of
      Nothing -> Left (InvalidJPEG "JPEG scan has no terminating marker")
      Just relative -> do
        let at = offset + relative
            next = at + 1 + BS.length (BS.takeWhile (== 255) (BS.drop (at + 1) bytes))
            body = BS.take (at - section) (BS.drop section bytes)
        require (next < BS.length bytes) "Truncated JPEG entropy marker"
        case byte bytes next of
          0 -> do
            require (next == at + 1) "Invalid entropy byte stuffing"
            go section (next + 1) complete
          tag | tag >= 0xd0 && tag <= 0xd7 ->
            go (next + 1) (next + 1) (Entropy body (Just (tag - 0xd0)) : complete)
          _ -> pure (reverse (Entropy body Nothing : complete), at)

describeFrame :: FrameMode -> BS.ByteString -> Either JpegError Frame
describeFrame mode body = do
  require (BS.length body >= 6) "Short JPEG frame header"
  unless (byte body 0 == 8) (Left (UnsupportedJPEG "JPEG sample precision"))
  let height = word16 body 1
      width = word16 body 3
      count = byte body 5
  unless (width > 0 && height > 0 && width <= 8192 && height <= 8192 && width * height <= 32000000) $
    Left (JPEGDimensions width height)
  unless (elem count [1, 3, 4]) (Left (UnsupportedJPEG "JPEG component count"))
  require (BS.length body == 6 + 3 * count) "Invalid JPEG frame component table"
  components <- traverse component [0 .. count - 1]
  let ids = map componentId components
      h = maximum (map horizontal components)
      v = maximum (map vertical components)
  require (IntSet.size (IntSet.fromList ids) == count) "Duplicate JPEG component ID"
  require (sum [horizontal c * vertical c | c <- components] <= 10) "Too many JPEG blocks per MCU"
  pure (Frame mode width height h v (ceilDivide width (8 * h)) (ceilDivide height (8 * v)) components)
  where
    component index = do
      let offset = 6 + 3 * index
          identifier = byte body offset
          sampling = byte body (offset + 1)
          h = shiftR sampling 4
          v = sampling .&. 15
          table = byte body (offset + 2)
      require (h >= 1 && h <= 4 && v >= 1 && v <= 4 && table <= 3) "Invalid JPEG sampling or quantization selector"
      pure (Component index identifier h v table)

describeScan :: Frame -> Tables -> BS.ByteString -> [Entropy] -> Either JpegError Scan
describeScan frame tables body entropy = do
  require (BS.length body >= 4) "Short JPEG scan header"
  let count = byte body 0
      offset = 1 + 2 * count
  require (count >= 1 && count <= length (frameComponents frame) && BS.length body == offset + 3) $
    "Invalid JPEG scan component count"
  let first = byte body offset
      final = byte body (offset + 1)
      approximation = byte body (offset + 2)
      high = shiftR approximation 4
      low = approximation .&. 15
      progressive = frameMode frame == Progressive
  if progressive
    then require (first <= final && final <= 63 && (if first == 0 then final == 0 else count == 1)
      && high <= 13 && low <= 13 && (high == 0 || high == low + 1)) "Invalid progressive JPEG scan parameters"
    else require (first == 0 && final == 63 && high == 0 && low == 0) "Invalid sequential JPEG scan parameters"
  components <- traverse (select progressive first high) [0 .. count - 1]
  let indices = [componentIndex component | ScanComponent component _ _ _ <- components]
  require (IntSet.size (IntSet.fromList indices) == count) "Duplicate component in JPEG scan"
  pure (Scan components first final high low (interval tables) entropy)
  where
    select progressive first high index = do
      let identifier = byte body (1 + 2 * index)
          selection = byte body (2 + 2 * index)
          dc = shiftR selection 4
          ac = selection .&. 15
      require (dc <= 3 && ac <= 3) "Invalid JPEG Huffman selector"
      component <- maybe (Left (InvalidJPEG "Unknown JPEG scan component")) pure $
        find ((== identifier) . componentId) (frameComponents frame)
      quantization <- lookupTable "quantization" (quantTables tables) (quantizationId component)
      dcTable <- if first == 0 && high == 0 then Just <$> lookupTable "DC" (codeTables tables) dc else pure Nothing
      acTable <- if not progressive || first /= 0 then Just <$> lookupTable "AC" (codeTables tables) (4 + ac) else pure Nothing
      pure (ScanComponent component quantization dcTable acTable)

lookupTable :: String -> IntMap.IntMap a -> Int -> Either JpegError a
lookupTable label tables key =
  maybe (Left (InvalidJPEG ("Missing JPEG " ++ label ++ " table"))) pure (IntMap.lookup key tables)

defineQuantization :: IntMap.IntMap Quantization -> BS.ByteString -> Either JpegError (IntMap.IntMap Quantization)
defineQuantization initial body = go 0 initial
  where
    go !offset tables
      | offset == BS.length body = pure tables
      | otherwise = do
          let selector = byte body offset
              precision = shiftR selector 4
              key = selector .&. 15
              count = if precision == 0 then 64 else 128
              next = offset + 1 + count
          require (precision <= 1 && key <= 3 && next <= BS.length body) "Invalid JPEG quantization table"
          let values = if precision == 0
                then [byte body (offset + 1 + index) | index <- [0 .. 63]]
                else [word16 body (offset + 1 + 2 * index) | index <- [0 .. 63]]
          require (all (> 0) values) "Zero JPEG quantization coefficient"
          go next (IntMap.insert key (listArray (0, 63) values) tables)

defineHuffman :: IntMap.IntMap Huffman -> BS.ByteString -> Either JpegError (IntMap.IntMap Huffman)
defineHuffman initial body = go 0 initial
  where
    go !offset tables
      | offset == BS.length body = pure tables
      | otherwise = do
          require (offset <= BS.length body - 17) "Truncated JPEG Huffman table"
          let selector = byte body offset
              kind = shiftR selector 4
              key = selector .&. 15
              counts = [byte body (offset + index) | index <- [1 .. 16]]
              count = sum counts
              next = offset + 17 + count
          require (kind <= 1 && key <= 3 && count >= 1 && count <= 256 && next <= BS.length body) $
            "Invalid JPEG Huffman table"
          book <- makeHuffman counts (BS.unpack (BS.take count (BS.drop (offset + 17) body)))
          go next (IntMap.insert (kind * 4 + key) book tables)

makeHuffman :: [Int] -> [Word8] -> Either JpegError Huffman
makeHuffman counts symbols = do
  (_, _, reversed) <- foldM next (0, 0, []) (zip [1 .. 16] counts)
  let ranges = reverse reversed
      dense = listArray (0, length symbols - 1) symbols
      starts = listArray (1, 16) [code | (_, code, _, _) <- ranges]
      ends = listArray (1, 16) [if count == 0 then -1 else code + count - 1 | (_, code, count, _) <- ranges]
      offsets = listArray (1, 16) [offset | (_, _, _, offset) <- ranges]
      indexed = IntMap.fromList (concatMap expand (take 8 ranges))
      lookup8 = listArray (0, 255) [IntMap.findWithDefault 0 index indexed | index <- [0 .. 255]]
  pure (Huffman lookup8 starts ends offsets dense)
  where
    next (!code, !offset, ranges) (width, count) = do
      let limit = shiftL 1 width
      require (code + count <= limit && (count == 0 || code + count < limit)) "Oversubscribed JPEG Huffman code"
      pure (shiftL (code + count) 1, offset + count, (width, code, count, offset) : ranges)
    expand (width, code, count, offset) =
      [ (index, fromIntegral (shiftL width 8 .|. fromIntegral symbol))
      | (position, symbol) <- zip [code .. code + count - 1] (take count (drop offset symbols))
      , index <- [shiftL position (8 - width) .. shiftL (position + 1) (8 - width) - 1] ]

require :: Bool -> String -> Either JpegError ()
require condition detail = unless condition (Left (InvalidJPEG detail))

byte :: BS.ByteString -> Int -> Int
byte bytes index = fromIntegral (BSU.unsafeIndex bytes index)

word16 :: BS.ByteString -> Int -> Int
word16 bytes index = shiftL (byte bytes index) 8 .|. byte bytes (index + 1)

ceilDivide :: Int -> Int -> Int
ceilDivide value divisor = quot (value + divisor - 1) divisor
