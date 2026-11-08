{-# LANGUAGE OverloadedStrings #-}

module GameIO.Image
  ( Image
  , ImageError (..)
  , decodeImage
  , decodeJpeg
  , rgbaImage
  , encodePng
  , imageSize
  , imageRGBA
  ) where

import Control.Monad (forM_)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits ((.&.), (.|.), countTrailingZeros, popCount, shiftL, shiftR, testBit, xor)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GameIO.Image.Deflate (adler32, inflateZlib)
import GameIO.Image.Jpeg qualified as Jpeg

data Image = Image !Int !Int !BS.ByteString

data ImageError
  = UnsupportedImage !String
  | InvalidImage !String
  | ImageDimensions !Int !Int
  deriving (Eq, Show)

imageSize :: Image -> (Int, Int)
imageSize (Image width height _) = (width, height)

-- | Row-major, top-left origin, straight-alpha RGBA8.
imageRGBA :: Image -> BS.ByteString
imageRGBA (Image _ _ bytes) = bytes

rgbaImage :: Int -> Int -> BS.ByteString -> Either ImageError Image
rgbaImage width height rgba = do
  dimensions width height
  require (BS.length rgba == width * height * 4) "incorrect RGBA byte length"
  pure (Image width height rgba)

-- | Lossless PNG suitable for pixel comparisons and captures. The encoder
-- uses stored deflate blocks to avoid making capture depend on a codec DLL.
encodePng :: Image -> BS.ByteString
encodePng (Image width height rgba) = serialize $
  Builder.byteString "\x89PNG\r\n\x1a\n" <>
  pngChunk "IHDR" header <>
  pngChunk "IDAT" compressed <>
  pngChunk "IEND" BS.empty
  where
    header = serialize (Builder.word32BE (fromIntegral width) <> Builder.word32BE (fromIntegral height)
      <> Builder.byteString (BS.pack [8,6,0,0,0]))
    stride = width * 4
    filtered = BSI.unsafeCreate (height * (stride + 1)) $ \output ->
      BSU.unsafeUseAsCString rgba $ \source ->
        let rows !row
              | row == height = pure ()
              | otherwise = do
                  let offset = row * (stride + 1)
                  pokeByteOff output offset (0 :: Word8)
                  copyBytes (plusPtr output (offset + 1)) (castPtr (plusPtr source (row * stride))) stride
                  rows (row + 1)
        in rows 0
    compressed = serialize (Builder.byteString (BS.pack [0x78,0x01]) <> stored filtered <> Builder.word32BE (adler32 filtered))
    stored bytes =
      let (block, rest) = BS.splitAt 65535 bytes
          size = fromIntegral (BS.length block)
          end = BS.null rest
          encoded = Builder.word8 (if end then 1 else 0) <> Builder.word16LE size
            <> Builder.word16LE (xor size 65535) <> Builder.byteString block
      in if end then encoded else encoded <> stored rest
    pngChunk kind payload = Builder.word32BE (fromIntegral (BS.length payload))
      <> Builder.byteString kind <> Builder.byteString payload <> Builder.word32BE (crc kind payload)
    serialize = BL.toStrict . Builder.toLazyByteString

decodeImage :: BS.ByteString -> Either ImageError Image
decodeImage bytes
  | BS.isPrefixOf "\x89PNG\r\n\x1a\n" bytes = decodePng bytes
  | BS.isPrefixOf "BM" bytes = decodeBmp bytes
  | BS.isPrefixOf "\xff\xd8" bytes = decodeJpeg bytes
  | otherwise = Left (UnsupportedImage "image file signature")

-- | Decode an 8-bit sequential or progressive Huffman JPEG as RGBA8.
decodeJpeg :: BS.ByteString -> Either ImageError Image
decodeJpeg bytes = do
  (width, height, rgba) <- either (Left . jpegError) Right (Jpeg.decodeJpegRGBA bytes)
  rgbaImage width height rgba
  where
    jpegError (Jpeg.InvalidJPEG detail) = InvalidImage detail
    jpegError (Jpeg.UnsupportedJPEG detail) = UnsupportedImage detail
    jpegError (Jpeg.JPEGDimensions width height) = ImageDimensions width height

dimensions :: Int -> Int -> Either ImageError ()
dimensions width height
  | width <= 0 || height <= 0 || width > 8192 || height > 8192 || width * height > 32000000 =
      Left (ImageDimensions width height)
  | otherwise = Right ()

require :: Bool -> String -> Either ImageError ()
require True _ = Right ()
require False reason = Left (InvalidImage reason)

takeAt :: BS.ByteString -> Int -> Int -> Either ImageError BS.ByteString
takeAt bytes start count
  | start < 0 || count < 0 || start > BS.length bytes - count = Left (InvalidImage "truncated image data")
  | otherwise = Right (BS.take count (BS.drop start bytes))

big, little :: BS.ByteString -> Int -> Int -> Either ImageError Int
big bytes offset count = BS.foldl' (\acc byte -> shiftL acc 8 .|. fromIntegral byte) 0 <$> takeAt bytes offset count
little bytes offset count = BS.foldr (\byte acc -> shiftL acc 8 .|. fromIntegral byte) 0 <$> takeAt bytes offset count

data Chunk = Chunk !BS.ByteString !BS.ByteString !Int

chunkAt :: BS.ByteString -> Int -> Either ImageError Chunk
chunkAt file start = do
  count <- big file start 4
  kind <- takeAt file (start + 4) 4
  body <- takeAt file (start + 8) count
  checksum <- big file (start + 8 + count) 4
  require (crc kind body == fromIntegral checksum) "PNG chunk CRC mismatch"
  pure (Chunk kind body (start + 12 + count))

crc :: BS.ByteString -> BS.ByteString -> Word32
crc kind body = xor 0xffffffff (BS.foldl' update (BS.foldl' update 0xffffffff kind) body)
  where
    update previous byte = xor (shiftR previous 8) (unsafeAt crcTable (fromIntegral ((xor previous (fromIntegral byte)) .&. 255)))

crcTable :: UArray Int Word32
crcTable = listArray (0,255) [foldr (\_ acc -> if testBit acc 0 then xor (shiftR acc 1) 0xedb88320 else shiftR acc 1) (fromIntegral i) [1 .. 8 :: Int] | i <- [0 .. 255 :: Int]]

data Chunks = Chunks !(Maybe BS.ByteString) !(Maybe BS.ByteString) ![BS.ByteString]

pngChunks :: BS.ByteString -> Int -> Either ImageError Chunks
pngChunks bytes start = collect start False (Chunks Nothing Nothing [])
  where
    collect offset endedIDAT (Chunks palette alpha dataChunks) = do
      Chunk kind body next <- chunkAt bytes offset
      case kind of
        "PLTE" -> do
          require (null dataChunks && maybe True (const False) palette) "unexpected PNG palette"
          require (BS.length body >= 3 && BS.length body <= 768 && rem (BS.length body) 3 == 0) "invalid PNG palette length"
          collect next endedIDAT (Chunks (Just body) alpha dataChunks)
        "tRNS" -> do
          require (null dataChunks && maybe True (const False) alpha) "unexpected PNG transparency chunk"
          collect next endedIDAT (Chunks palette (Just body) dataChunks)
        "IDAT" -> do
          require (not endedIDAT) "PNG IDAT chunks must be consecutive"
          collect next False (Chunks palette alpha (body : dataChunks))
        "IEND" -> do
          require (not (null dataChunks) && BS.null body && next == BS.length bytes) "PNG IEND or missing image data"
          pure (Chunks palette alpha (reverse dataChunks))
        _ | testBit (BS.index kind 0) 5 -> collect next (not (null dataChunks)) (Chunks palette alpha dataChunks)
          | otherwise -> Left (UnsupportedImage ("PNG critical chunk " ++ show kind))

data ScanPass = ScanPass
  { passOrigin :: !(Int, Int)
  , passStep :: !(Int, Int)
  , passExtent :: !(Int, Int)
  }

decodePng :: BS.ByteString -> Either ImageError Image
decodePng bytes = do
  Chunk kind header offset <- chunkAt bytes 8
  require (kind == "IHDR" && BS.length header == 13) "PNG starts without a valid IHDR"
  width <- big header 0 4
  height <- big header 4 4
  dimensions width height
  let depth = fromIntegral (BS.index header 8)
      color = BS.index header 9
      interlace = BS.index header 12
  require (BS.index header 10 == 0 && BS.index header 11 == 0) "PNG compression or filter method"
  (channels, depths) <- case color of
    0 -> pure (1, [1,2,4,8,16])
    2 -> pure (3, [8,16])
    3 -> pure (1, [1,2,4,8])
    4 -> pure (2, [8,16])
    6 -> pure (4, [8,16])
    _ -> Left (UnsupportedImage "PNG color type")
  require (elem depth depths) "PNG sample depth for color type"
  geometry <- case interlace of
    0 -> pure [(0, 0, 1, 1)]
    1 -> pure [(0,0,8,8), (4,0,8,8), (0,4,4,8), (2,0,4,4), (0,2,2,4), (1,0,2,2), (0,1,1,2)]
    _ -> Left (UnsupportedImage "PNG interlace method")
  Chunks palette transparency blocks <- pngChunks bytes offset
  layout <- pngLayout color depth palette transparency
  let passes =
        [ ScanPass (x,y) (dx,dy) (w,h)
        | (x,y,dx,dy) <- geometry
        , let w = div (width - x + dx - 1) dx
              h = div (height - y + dy - 1) dy
        , w > 0 && h > 0
        ]
      stride w = div (w * channels * depth + 7) 8
      filterBytesPerPixel = div (channels * depth + 7) 8
      expanded pass = let (w,h) = passExtent pass in h * (stride w + 1)
      decodePass input pass = do
        let (w,h) = passExtent pass
            (scanlines, rest) = BS.splitAt (expanded pass) input
        raw <- unfilter (stride w) filterBytesPerPixel h scanlines
        rgba <- pngPixels layout channels depth w h (stride w) raw
        pure ((pass, rgba), rest)
      decodePasses _ [] = pure []
      decodePasses input (pass : rest) = do
        (decoded, remaining) <- decodePass input pass
        (decoded :) <$> decodePasses remaining rest
  filtered <- either (Left . InvalidImage) Right (inflateZlib (sum (map expanded passes)) (BS.concat blocks))
  decoded <- decodePasses filtered passes
  let rgba = case decoded of
        [(ScanPass (0,0) (1,1) _, pixels)] -> pixels
        _ -> assemblePasses width height decoded
  pure (Image width height rgba)

-- The seven validated pass geometries cover each destination pixel once.
assemblePasses :: Int -> Int -> [(ScanPass, BS.ByteString)] -> BS.ByteString
assemblePasses width height passes = BSI.unsafeCreate (width * height * 4) $ \output ->
  forM_ passes $ \(pass, rgba) ->
    BS.useAsCString rgba $ \source -> do
      let (x0,y0) = passOrigin pass
          (dx,dy) = passStep pass
          (w,h) = passExtent pass
          row !y
            | y >= h = pure ()
            | otherwise = do
                let first = (y0 + y * dy) * width + x0
                    column !x
                      | x >= w = pure ()
                      | otherwise = do
                          copyBytes (plusPtr output (4 * (first + x * dx)))
                            (castPtr (plusPtr source (4 * (y * w + x)))) 4
                          column (x + 1)
                column 0
                row (y + 1)
      row 0


unfilter :: Int -> Int -> Int -> BS.ByteString -> Either ImageError BS.ByteString
unfilter stride bpp height bytes = do
  require (all (\row -> BSU.unsafeIndex bytes (row * (stride + 1)) <= 4) [0 .. height - 1]) "unknown PNG scanline filter"
  pure (BSI.unsafeCreate (stride * height) (recoverRows stride bpp height bytes))

-- The caller validates the filter bytes and exact expanded length. Previously
-- written bytes in this local buffer are the left and upper neighbors.
recoverRows :: Int -> Int -> Int -> BS.ByteString -> Ptr Word8 -> IO ()
recoverRows stride bpp height bytes output = BSU.unsafeUseAsCString bytes $ \source ->
  let row !y
        | y >= height = pure ()
        | otherwise = do
            let prefix = y * (stride + 1)
                method = BSU.unsafeIndex bytes prefix
                first = y * stride
                pixel !x
                  | x >= stride = pure ()
                  | otherwise = do
                      let encoded = BSU.unsafeIndex bytes (prefix + 1 + x)
                          index = first + x
                      a <- if x >= bpp && method /= 2 then peekByteOff output (index - bpp) else pure 0
                      b <- if y > 0 && method /= 1 then peekByteOff output (index - stride) else pure 0
                      predictor <- case method of
                        1 -> pure a
                        2 -> pure b
                        3 -> pure (fromIntegral (div (fromIntegral a + fromIntegral b :: Int) 2))
                        _ -> do
                          c <- if x >= bpp && y > 0 then peekByteOff output (index - bpp - stride) else pure 0
                          pure (paeth a b c)
                      pokeByteOff output index (encoded + predictor)
                      pixel (x + 1)
            if method == 0
              then copyBytes (plusPtr output first) (castPtr (plusPtr source (prefix + 1))) stride
              else pixel 0
            row (y + 1)
  in row 0


paeth :: Word8 -> Word8 -> Word8 -> Word8
paeth a b c
  | da <= db && da <= dc = a
  | db <= dc = b
  | otherwise = c
  where
    x = fromIntegral a :: Int
    y = fromIntegral b
    z = fromIntegral c
    p = x + y - z
    da = abs (p - x)
    db = abs (p - y)
    dc = abs (p - z)

data PngLayout
  = Luma !(Maybe Int)
  | TrueColor !(Maybe (Int, Int, Int))
  | Palette !BS.ByteString !(Maybe BS.ByteString)
  | LumaAlpha
  | TrueColorAlpha

pngLayout :: Word8 -> Int -> Maybe BS.ByteString -> Maybe BS.ByteString -> Either ImageError PngLayout
pngLayout color depth palette transparency = case color of
  0 -> Luma <$> traverse (\payload -> size 2 payload >> big payload 0 2) transparency
  2 -> TrueColor <$> traverse (\payload -> size 6 payload >> (,,) <$> big payload 0 2 <*> big payload 2 2 <*> big payload 4 2) transparency
  3 -> case palette of
    Nothing -> Left (InvalidImage "indexed PNG lacks a palette")
    Just colors -> do
      let entries = div (BS.length colors) 3
      require (entries <= shiftL 1 depth) "PNG palette exceeds sample depth"
      require (maybe True ((<= entries) . BS.length) transparency) "PNG transparency exceeds palette"
      pure (Palette colors transparency)
  4 -> LumaAlpha <$ require (maybe True (const False) transparency) "PNG already has alpha samples"
  6 -> TrueColorAlpha <$ require (maybe True (const False) transparency) "PNG already has alpha samples"
  _ -> Left (UnsupportedImage "PNG color type")
  where size n payload = require (BS.length payload == n) "PNG transparency sample length"

pngPixels :: PngLayout -> Int -> Int -> Int -> Int -> Int -> BS.ByteString -> Either ImageError BS.ByteString
pngPixels layout channels depth width height stride bytes = case layout of
  Luma transparency -> pure $ expand $ \i ->
    let sample = value i 0
        g = byte sample
    in (g,g,g,alphaFor (Just sample == transparency))
  TrueColor transparency ->
    pure $ expand $ \i ->
      let r = value i 0
          g = value i 1
          b = value i 2
      in (byte r, byte g, byte b, alphaFor (Just (r,g,b) == transparency))
  Palette colors transparency -> do
    let entries = div (BS.length colors) 3
    require (all (\i -> value i 0 < entries) [0 .. count - 1]) "PNG palette index outside palette"
    pure $ expand $ \i ->
      let entry = value i 0
          a = case transparency of
            Just values | entry < BS.length values -> BSU.unsafeIndex values entry
            _ -> 255
          first = 3 * entry
      in (BSU.unsafeIndex colors first, BSU.unsafeIndex colors (first + 1), BSU.unsafeIndex colors (first + 2), a)
  LumaAlpha -> pure $ expand $ \i -> let g = byte (value i 0) in (g,g,g,byte (value i 1))
  TrueColorAlpha
    | depth == 8 -> pure bytes
    | otherwise -> pure $ expand $ \i -> (byte (value i 0), byte (value i 1), byte (value i 2), byte (value i 3))
  where
    count = width * height
    expand = rgbaBytes count
    alphaFor transparent = if transparent then 0 else 255
    byte sample
      | depth < 8 = fromIntegral (div (sample * 255) (shiftL 1 depth - 1))
      | depth == 8 = fromIntegral sample
      | otherwise = fromIntegral (shiftR sample 8)
    value i channel =
      let (row, column) = divMod i width
          start = row * stride
          sample = column * channels + channel
      in case depth of
        8 -> fromIntegral (BSU.unsafeIndex bytes (start + sample))
        16 ->
          let offset = start + 2 * sample
          in shiftL (fromIntegral (BSU.unsafeIndex bytes offset)) 8 .|. fromIntegral (BSU.unsafeIndex bytes (offset + 1))
        _ -> packedSample bytes start depth sample

-- Sub-byte samples are packed most-significant first. Row padding is skipped
-- by the caller rather than becoming a sample at the next row.
packedSample :: BS.ByteString -> Int -> Int -> Int -> Int
packedSample bytes start depth sample =
  let (offset, bit) = divMod (sample * depth) 8
      word = fromIntegral (BSU.unsafeIndex bytes (start + offset))
  in shiftR word (8 - bit - depth) .&. (shiftL 1 depth - 1)


rgbaBytes :: Int -> (Int -> (Word8, Word8, Word8, Word8)) -> BS.ByteString
rgbaBytes count sample = BSI.unsafeCreate (4 * count) $ \output ->
  let write !i
        | i >= count = pure ()
        | otherwise = do
            let (r,g,b,a) = sample i
                offset = 4 * i
            pokeByteOff output offset r
            pokeByteOff output (offset + 1) g
            pokeByteOff output (offset + 2) b
            pokeByteOff output (offset + 3) a
            write (i + 1)
  in write 0


decodeBmp :: BS.ByteString -> Either ImageError Image
decodeBmp bytes = do
  start <- little bytes 10 4
  header <- little bytes 14 4
  let core = header == 12
      paletteStart = 14 + header
  require ((core || header >= 40) && start >= paletteStart) "invalid BMP DIB header"
  (width, signedHeight, planes, depth, compression, declaredColors) <-
    if core
      then (,,,,,) <$> little bytes 18 2 <*> little bytes 20 2 <*> little bytes 22 2 <*> little bytes 24 2 <*> pure 0 <*> pure 0
      else (,,,,,) <$> (signed <$> little bytes 18 4) <*> (signed <$> little bytes 22 4)
        <*> little bytes 26 2 <*> little bytes 28 2 <*> little bytes 30 4 <*> little bytes 46 4
  let height = abs signedHeight
      rowBytes = div (width * depth + 31) 32 * 4
      count = width * height
      rowAndColumn i =
        let (y,x) = divMod i width
            fileRow = if signedHeight < 0 then y else height - 1 - y
        in (fileRow * rowBytes, x)
  dimensions width height
  require (planes == 1) "invalid BMP plane count"
  if (compression == 0 && elem depth [1,4,8,16,24,32]) || (compression == 3 && elem depth [16,32])
    then pure ()
    else Left (UnsupportedImage "BMP compression or bit depth")
  body <- takeAt bytes start (height * rowBytes)
  pixel <- case depth of
    24 -> pure $ \i ->
      let (row,x) = rowAndColumn i
          offset = row + x * 3
      in (BSU.unsafeIndex body (offset + 2), BSU.unsafeIndex body (offset + 1), BSU.unsafeIndex body offset, 255)
    _ | depth < 16 -> do
          let entryBytes = if core then 3 else 4
              available = div (start - paletteStart) entryBytes
              entries = if declaredColors == 0 then min available (shiftL 1 depth) else declaredColors
              index i = let (row,x) = rowAndColumn i in packedSample body row depth x
          require (entries >= 1 && entries <= shiftL 1 depth && entries <= available) "invalid BMP palette length"
          colors <- takeAt bytes paletteStart (entries * entryBytes)
          require (all ((< entries) . index) [0 .. count - 1]) "BMP palette index outside palette"
          pure $ \i ->
            let offset = entryBytes * index i
            in (BSU.unsafeIndex colors (offset + 2), BSU.unsafeIndex colors (offset + 1), BSU.unsafeIndex colors offset, 255)
      | otherwise -> do
          (red, green, blue, alpha) <- bmpMasks bytes header start depth compression
          let channels = div depth 8
              value i =
                let (row,x) = rowAndColumn i
                    offset = row + x * channels
                    readPart part = shiftL (fromIntegral (BSU.unsafeIndex body (offset + part))) (8 * part)
                    low = readPart 0 .|. readPart 1
                in if depth == 16 then low else low .|. readPart 2 .|. readPart 3
              alphaByte
                | compression == 0 && depth == 32
                , all ((== 0) . maskByte alpha . value) [0 .. count - 1] = const 255
                | otherwise = maskByte alpha
          pure $ \i ->
            let word = value i
            in (maskByte red word, maskByte green word, maskByte blue word, alphaByte word)
  pure (Image width height (rgbaBytes count pixel))
  where signed = fromIntegral . (fromIntegral :: Int -> Int32)

data ChannelMask = NoMask | ChannelMask !Int !Int !Word32

bmpMasks :: BS.ByteString -> Int -> Int -> Int -> Int -> Either ImageError (ChannelMask, ChannelMask, ChannelMask, ChannelMask)
bmpMasks bytes header start depth compression = do
  (r,g,b,a) <- if compression == 3 then do
    require (start >= 66) "truncated BMP masks"
    red <- maskAt 54
    green <- maskAt 58
    blue <- maskAt 62
    alpha <- if header >= 56 then maskAt 66 else pure 0
    pure (red, green, blue, alpha)
    else pure $ if depth == 16 then (0x7c00, 0x03e0, 0x001f, 0) else (0xff0000, 0xff00, 0xff, 0xff000000)
  require (r /= 0 && g /= 0 && b /= 0 && r .&. g == 0 && r .&. b == 0 && g .&. b == 0 && a .&. (r .|. g .|. b) == 0)
    "overlapping or empty BMP color mask"
  (,,,) <$> channel r <*> channel g <*> channel b <*> channel a
  where
    maskAt offset = fromIntegral <$> little bytes offset 4
    channel 0 = pure NoMask
    channel mask = do
      let offset = countTrailingZeros mask
          width = popCount mask
          lowMask = shiftR mask offset
      require (width <= 8 && offset + width <= depth && lowMask == shiftL 1 width - 1) "non-contiguous or wide BMP mask"
      pure (ChannelMask offset width mask)

maskByte :: ChannelMask -> Word32 -> Word8
maskByte NoMask _ = 255
maskByte (ChannelMask offset width mask) word = fromIntegral (repeatBits width aligned)
  where
    aligned = shiftL (shiftR (word .&. mask) offset) (8 - width)
    repeatBits copied bits
      | copied >= 8 = bits
      | otherwise = repeatBits (copied + width) (bits .|. shiftR aligned copied)
