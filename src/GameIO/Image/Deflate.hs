module GameIO.Image.Deflate (inflateZlib, adler32) where

import Control.Monad (foldM)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit, xor)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.IntMap.Strict qualified as IntMap
import Data.Word (Word8, Word32)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

data Tree = Empty | Leaf !Int | Branch !Tree !Tree
data Tables = Tables !Tree !Tree

type Result a = Either String a

-- The caller derives the exact expanded length from the validated PNG header.
-- The decoder cannot allocate or fill more than that amount.
inflateZlib :: Int -> BS.ByteString -> Result BS.ByteString
inflateZlib expected bytes
  | expected <= 0 = Left "invalid expanded length"
  | BS.length bytes < 6 = Left "truncated zlib stream"
  | cmf .&. 15 /= 8 || shiftR cmf 4 > 7 || rem (cmf * 256 + flg) 31 /= 0 = Left "invalid zlib header"
  | testBit flg 5 = Left "preset zlib dictionary"
  | otherwise = do
      -- Only the immutable, fully decoded buffer escapes. Every block checks
      -- the exact output capacity before writing literals or copied bytes.
      let (body, status) = BSI.unsafeCreateUptoN' expected $ \output -> do
            result <- blocks expected compressed output 0 0
            pure $ case result of
              Left reason -> (0, Left reason)
              Right (used, written)
                | written /= expected -> (0, Left "incorrect expanded length")
                | div (used + 7) 8 /= BS.length compressed -> (0, Left "trailing deflate data")
                | otherwise -> (written, Right ())
      status
      if adler32 body == advertised then Right body else Left "incorrect zlib Adler-32"
  where
    cmf = fromIntegral (BS.index bytes 0) :: Int
    flg = fromIntegral (BS.index bytes 1) :: Int
    compressed = BS.take (BS.length bytes - 6) (BS.drop 2 bytes)
    advertised = BS.foldl' (\acc byte -> shiftL acc 8 .|. fromIntegral byte) 0 (BS.drop (BS.length bytes - 4) bytes)

adler32 :: BS.ByteString -> Word32
adler32 bytes = sumBytes 0 1 0
  where
    lengthBytes = BS.length bytes
    sumBytes !index !low !high
      | index == lengthBytes = shiftL high 16 .|. low
      | otherwise =
          let a = low + fromIntegral (BSU.unsafeIndex bytes index)
              nextLow = if a >= 65521 then a - 65521 else a
              b = high + nextLow
              nextHigh = if b >= 65521 then b - 65521 else b
          in sumBytes (index + 1) nextLow nextHigh

bits :: BS.ByteString -> Int -> Int -> Result (Int, Int)
bits bytes offset width
  | offset < 0 || offset + width > BS.length bytes * 8 = Left "truncated deflate bit stream"
  | width == 0 = Right (0, offset)
  | otherwise =
      let (start, skip) = divMod offset 8
          byte i = if i < BS.length bytes then fromIntegral (BSU.unsafeIndex bytes i) else 0
          word = byte start .|. shiftL (byte (start + 1)) 8 .|. shiftL (byte (start + 2)) 16
      in Right (shiftR word skip .&. (shiftL 1 width - 1), offset + width)

huffman :: [Int] -> Result Tree
huffman lengths = snd <$> foldM assign (firstCodes, Empty) (zip [0 ..] lengths)
  where
    counts :: IntMap.IntMap Int
    counts = IntMap.fromListWith (+) [(width, 1) | width <- lengths, width > 0]
    (_, firstCodes) = foldl nextCode (0, IntMap.empty) [1 .. 15]
    nextCode (previous, starts) width =
      let first = 2 * (previous + IntMap.findWithDefault 0 (width - 1) counts)
      in (first, IntMap.insert width first starts)
    assign pair (_, 0) = pure pair
    assign (codes, tree) (symbol, width) = do
      code <- maybe (Left "invalid deflate code width") Right (IntMap.lookup width codes)
      if code >= shiftL 1 width then Left "oversubscribed deflate tree" else do
        new <- insert symbol code (width - 1) tree
        pure (IntMap.insert width (code + 1) codes, new)
    insert symbol _ (-1) Empty = Right (Leaf symbol)
    insert _ _ (-1) _ = Left "duplicate deflate code"
    insert symbol code depth Empty = insert symbol code depth (Branch Empty Empty)
    insert symbol code depth (Branch zero one)
      | testBit code depth = Branch zero <$> insert symbol code (depth - 1) one
      | otherwise = flip Branch one <$> insert symbol code (depth - 1) zero
    insert _ _ _ (Leaf _) = Left "overlapping deflate codes"

symbolAt :: BS.ByteString -> Tree -> Int -> Result (Int, Int)
symbolAt bytes = descend
  where
    end = BS.length bytes * 8
    descend Empty _ = Left "invalid deflate code"
    descend (Leaf symbol) offset = Right (symbol, offset)
    descend (Branch zero one) offset
      | offset >= end = Left "truncated deflate Huffman code"
      | otherwise =
          let (index, bit) = divMod offset 8
              branch = if testBit (BSU.unsafeIndex bytes index) bit then one else zero
          in descend branch (offset + 1)

fixedTables :: Result Tables
fixedTables = Tables
  <$> huffman (replicate 144 8 ++ replicate 112 9 ++ replicate 24 7 ++ replicate 8 8)
  <*> huffman (replicate 32 5)

dynamicTables :: BS.ByteString -> Int -> Result (Tables, Int)
dynamicTables bytes offset = do
  (literalCount, a) <- bits bytes offset 5
  (distanceCount, b) <- bits bytes a 5
  (codeCount, c) <- bits bytes b 4
  if literalCount > 29 then Left "invalid deflate HLIT" else pure ()
  (lengthMap, d) <- foldM readLength (IntMap.empty, c) (take (codeCount + 4) codeOrder)
  codes <- huffman [IntMap.findWithDefault 0 i lengthMap | i <- [0..18]]
  (lengths, e) <- readLengths codes (literalCount + distanceCount + 258) d []
  let (literals, distances) = splitAt (literalCount + 257) lengths
  case drop 256 literals of
    0 : _ -> Left "deflate literal tree lacks block end"
    [] -> Left "truncated literal tree"
    _ -> do
      tables <- Tables <$> huffman literals <*> huffman distances
      pure (tables, e)
  where
    codeOrder = [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]
    readLength (entries, i) code = do
      (width, next) <- bits bytes i 3
      pure (IntMap.insert code width entries, next)
    readLengths _ 0 i previous = Right (reverse previous, i)
    readLengths codes remaining i previous = do
      (symbol, next) <- symbolAt bytes codes i
      case symbol of
        16 -> case previous of
          [] -> Left "deflate repeats a missing code length"
          value : _ -> repeated codes remaining next previous value 3 2
        17 -> repeated codes remaining next previous 0 3 3
        18 -> repeated codes remaining next previous 0 11 7
        _ -> readLengths codes (remaining - 1) next (symbol : previous)
    repeated codes remaining i previous value start width = do
      (extra, next) <- bits bytes i width
      let count = start + extra
      if count > remaining then Left "deflate length repeat exceeds tree" else
        readLengths codes (remaining - count) next (replicate count value ++ previous)

blocks :: Int -> BS.ByteString -> Ptr Word8 -> Int -> Int -> IO (Result (Int, Int))
blocks capacity bytes output = loop
  where
    loop offset written = case bits bytes offset 3 of
      Left reason -> pure (Left reason)
      Right (flags, afterHeader) -> do
        decoded <- case shiftR flags 1 of
          0 -> stored afterHeader written
          1 -> either (pure . Left) (\tables -> coded tables afterHeader written) fixedTables
          2 -> either (pure . Left) (\(tables, afterTrees) -> coded tables afterTrees written) (dynamicTables bytes afterHeader)
          _ -> pure (Left "reserved deflate block type")
        case decoded of
          Right (next, count) | not (testBit flags 0) -> loop next count
          _ -> pure decoded
    stored offset written = case storedRange offset written of
      Left reason -> pure (Left reason)
      Right (first, len) -> BSU.unsafeUseAsCString bytes $ \source -> do
        copyBytes (plusPtr output written) (castPtr (plusPtr source first)) len
        pure (Right ((first + len) * 8, written + len))
    storedRange offset written = do
      let aligned = div (offset + 7) 8 * 8
      (len, endLength) <- bits bytes aligned 16
      (inverted, firstBit) <- bits bytes endLength 16
      let first = div firstBit 8
      if xor len inverted /= 65535 || len > capacity - written || len > BS.length bytes - first
        then Left "invalid stored deflate block"
        else Right (first, len)
    coded tables@(Tables literals distances) offset written = case symbolAt bytes literals offset of
      Left reason -> pure (Left reason)
      Right (256, next) -> pure (Right (next, written))
      Right (literal, next) | literal < 256 ->
        if written >= capacity then pure (Left "deflate output exceeds its image") else do
          pokeByteOff output written (fromIntegral literal :: Word8)
          coded tables next (written + 1)
      Right (symbol, next) -> case backReference bytes distances symbol next written capacity of
        Left reason -> pure (Left reason)
        Right (len, distance, nextBit) -> do
          let first = written - distance
              copyOverlap !i
                | i == len = pure ()
                | otherwise = do
                    byte <- peekByteOff output (first + i) :: IO Word8
                    pokeByteOff output (written + i) byte
                    copyOverlap (i + 1)
          if len <= distance
            then copyBytes (plusPtr output written) (plusPtr output first) len
            else copyOverlap 0
          coded tables nextBit (written + len)

backReference :: BS.ByteString -> Tree -> Int -> Int -> Int -> Int -> Result (Int, Int, Int)
backReference bytes distances lengthSymbol offset written capacity
  | lengthSymbol < 257 || lengthSymbol > 285 = Left "reserved deflate length code"
  | otherwise = do
      let i = lengthSymbol - 257
      (delta, next) <- bits bytes offset (lengthExtra ! i)
      (distanceSymbol, nextDistance) <- symbolAt bytes distances next
      if distanceSymbol >= 30 then Left "reserved deflate distance code" else do
        let len = lengthBase ! i + delta
        (extra, following) <- bits bytes nextDistance (distanceExtra ! distanceSymbol)
        let distance = distanceBase ! distanceSymbol + extra
        if len > capacity - written || distance > written
          then Left "deflate back reference exceeds buffer"
          else Right (len, distance, following)

lengthBase, lengthExtra, distanceBase, distanceExtra :: UArray Int Int
lengthBase = listArray (0,28) [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258]
lengthExtra = listArray (0,28) (replicate 8 0 ++ concatMap (replicate 4) [1..5] ++ [0])
distanceBase = listArray (0,29) [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577]
distanceExtra = listArray (0,29) (replicate 4 0 ++ concatMap (replicate 2) [1..13])
