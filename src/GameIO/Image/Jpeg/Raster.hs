module GameIO.Image.Jpeg.Raster (Plane, renderPlane, renderRGBA) where

import Control.Monad.ST (ST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray)
import Data.Array.Unboxed (UArray, listArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int16)
import Data.Word (Word8)
import Foreign.Storable (pokeElemOff)
import GameIO.Image.Jpeg.Syntax

data Plane = Plane !Int !Int !Int !Int !(UArray Int Word8)

renderPlane
  :: Frame -> Component -> Int -> STUArray s Int Int16 -> Quantization -> ST s Plane
renderPlane frame component stride coefficients quantization = do
  let h = horizontal component
      v = vertical component
      width = ceilDivide (frameWidth frame * h) (maxHorizontal frame)
      height = ceilDivide (frameHeight frame * v) (maxVertical frame)
  output <- sampleArray (width * height)
  spatial <- transformArray
  horizontalPass <- transformArray
  -- SOF geometry bounds block indices. The raster loops use only the visible
  -- part of edge blocks, and each transform index belongs to its 8×8 block.
  forIndex (ceilDivide height 8) $ \row ->
    forIndex (ceilDivide width 8) $ \column -> do
      let index = 64 * (row * stride + column)
          x0 = column * 8
          y0 = row * 8
          columns = min 8 (width - x0)
          rows = min 8 (height - y0)
      dc <- unsafeRead coefficients index
      let dcValue = fromIntegral dc * unsafeAt quantization 0
          load !coefficient !maxU !maxV
            | coefficient >= 64 = pure (maxU, maxV)
            | otherwise = do
                raw <- unsafeRead coefficients (index + coefficient)
                let natural = unsafeAt zigzag coefficient
                    unquantized = fromIntegral raw * unsafeAt quantization coefficient
                unsafeWrite spatial natural (fromIntegral unquantized)
                if raw == 0 then load (coefficient + 1) maxU maxV
                  else let (vIndex, u) = quotRem natural 8
                       in load (coefficient + 1) (max maxU u) (max maxV vIndex)
      (maxU, maxV) <- load 1 0 0
      if maxU > 0 || maxV > 0 then do
        unsafeWrite spatial 0 (fromIntegral dcValue)
        forIndex (maxV + 1) $ \frequency ->
          forIndex columns $ \x -> do
            value <- dot (maxU + 1) $ \u -> do
              coefficient <- unsafeRead spatial (8 * frequency + u)
              pure (coefficient * unsafeAt idctBasis (8 * x + u))
            unsafeWrite horizontalPass (8 * frequency + x) value
        forIndex rows $ \y ->
          forIndex columns $ \x -> do
            value <- dot (maxV + 1) $ \vIndex -> do
              coefficient <- unsafeRead horizontalPass (8 * vIndex + x)
              pure (coefficient * unsafeAt idctBasis (8 * y + vIndex))
            unsafeWrite output ((y0 + y) * width + x0 + x) (toSample value)
      else do
        let sample = fromIntegral (clamp8 (128 + div (dcValue + 4) 8))
        forIndex rows $ \y ->
          forIndex columns $ \x ->
            unsafeWrite output ((y0 + y) * width + x0 + x) sample
  -- The plane is complete and leaves the ST region without further writes.
  Plane h v width height <$> unsafeFreeze output

sampleArray :: Int -> ST s (STUArray s Int Word8)
sampleArray count = newArray (0, count - 1) 0

transformArray :: ST s (STUArray s Int Double)
transformArray = newArray (0, 63) 0

forIndex :: Int -> (Int -> ST s ()) -> ST s ()
forIndex end action = go 0
  where
    go !index
      | index >= end = pure ()
      | otherwise = action index >> go (index + 1)
{-# INLINE forIndex #-}

dot :: Int -> (Int -> ST s Double) -> ST s Double
dot end term = go 0 0
  where
    go !index !total
      | index >= end = pure total
      | otherwise = term index >>= go (index + 1) . (total +)
{-# INLINE dot #-}

-- T.81 Annex A, separable inverse transform with the orthonormal DC term.
idctBasis :: UArray Int Double
idctBasis = listArray (0, 63)
  [ if frequency == 0 then 0.5 / sqrt 2
    else 0.5 * cos (fromIntegral ((2 * x + 1) * frequency) * pi / 16)
  | x <- [0 .. 7 :: Int], frequency <- [0 .. 7 :: Int] ]

zigzag :: UArray Int Int
zigzag = listArray (0, 63)
  [ 0, 1, 8,16, 9, 2, 3,10
  ,17,24,32,25,18,11, 4, 5
  ,12,19,26,33,40,48,41,34
  ,27,20,13, 6, 7,14,21,28
  ,35,42,49,56,57,50,43,36
  ,29,22,15,23,30,37,44,51
  ,58,59,52,45,38,31,39,46
  ,53,60,61,54,47,55,62,63 ]

toSample :: Double -> Word8
toSample value = fromIntegral (clamp8 (floor (128.5 + value)))
{-# INLINE toSample #-}

clamp8 :: Int -> Int
clamp8 = max 0 . min 255
{-# INLINE clamp8 #-}

renderRGBA :: Frame -> Maybe Int -> [Plane] -> Either JpegError BS.ByteString
renderRGBA frame adobe planes = case map (sampleAt frame) planes of
  [luma] -> pure $ writeImage (\x y -> let shade = luma x y in (shade, shade, shade))
  [first, second, third]
    | adobe == Just 0 || map componentId (frameComponents frame) == [82, 71, 66] ->
        pure $ writeImage (\x y -> (first x y, second x y, third x y))
    | otherwise -> pure $ writeImage (\x y -> ycbcr (first x y) (second x y) (third x y))
  [first, second, third, key] -> case adobe of
    Just 0 -> pure $ writeImage $ \x y ->
      let k = key x y
      in (multiply (first x y) k, multiply (second x y) k, multiply (third x y) k)
    Just 2 -> pure $ writeImage $ \x y ->
      let (r, g, b) = ycbcr (first x y) (second x y) (third x y)
          k = key x y
      in (multiply (255 - r) k, multiply (255 - g) k, multiply (255 - b) k)
    Nothing -> pure $ writeImage $ \x y ->
      let k = 255 - key x y
      in (multiply (255 - first x y) k, multiply (255 - second x y) k, multiply (255 - third x y) k)
    _ -> Left (UnsupportedJPEG "Adobe JPEG color transform")
  _ -> Left (InvalidJPEG "Unexpected JPEG plane count")
  where
    width = frameWidth frame
    height = frameHeight frame
    -- The freshly allocated byte buffer has no aliases during the fill.
    writeImage = rgbaBytes width height
    multiply a b = quot (a * b + 127) 255

rgbaBytes :: Int -> Int -> (Int -> Int -> (Int, Int, Int)) -> BS.ByteString
rgbaBytes width height color = BSI.unsafeCreate (width * height * 4) $ \output ->
  let fill !x !y !index
        | y >= height = pure ()
        | x >= width = fill 0 (y + 1) index
        | otherwise = do
            let (!r, !g, !b) = color x y
            pokeElemOff output index (fromIntegral r :: Word8)
            pokeElemOff output (index + 1) (fromIntegral g :: Word8)
            pokeElemOff output (index + 2) (fromIntegral b :: Word8)
            pokeElemOff output (index + 3) (255 :: Word8)
            fill (x + 1) y (index + 4)
  in fill 0 0 0
{-# INLINE rgbaBytes #-}

-- Sample-centred upsampling. Clamp to the actual component extent at edges,
-- so padding introduced by the last MCU cannot influence visible pixels.
sampleAt :: Frame -> Plane -> Int -> Int -> Int
sampleAt frame (Plane h v width height samples)
  | h == maxHorizontal frame && v == maxVertical frame = at
  | otherwise = \x y ->
      let (x0, wx) = quotRem (unsafeAt xPlan x) 8
          (y0, wy) = quotRem (unsafeAt yPlan y) 8
          x1 = if wx == 0 then x0 else x0 + 1
          y1 = if wy == 0 then y0 else y0 + 1
          top = at x0 y0 * (dx - wx) + at x1 y0 * wx
          bottom = at x0 y1 * (dx - wx) + at x1 y1 * wx
          combined = top * (dy - wy) + bottom * wy
      in quot (combined + quot (dx * dy) 2) (dx * dy)
  where
    dx = 2 * maxHorizontal frame
    dy = 2 * maxVertical frame
    at x y = fromIntegral (unsafeAt samples (y * width + x))
    xPlan = axisPlan width h (maxHorizontal frame) (frameWidth frame)
    yPlan = axisPlan height v (maxVertical frame) (frameHeight frame)
{-# INLINE sampleAt #-}

axisPlan :: Int -> Int -> Int -> Int -> UArray Int Int
axisPlan extent factor maximumFactor count = listArray (0, count - 1) (map point [0 .. count - 1])
  where
    -- The low three bits hold the fractional weight. A zero weight also
    -- represents a clamped edge, with both lookups at the visible sample.
    point coordinate =
      let (base, weight) = divMod ((2 * coordinate + 1) * factor - maximumFactor) (2 * maximumFactor)
      in if base < 0 then 0
         else if base >= extent - 1 then 8 * (extent - 1)
         else 8 * base + weight

ycbcr :: Int -> Int -> Int -> (Int, Int, Int)
ycbcr y cb cr =
  let b = cb - 128
      r = cr - 128
      rounded change = clamp8 (y + div (change + 32768) 65536)
  in (rounded (91881 * r), rounded (negate (22554 * b + 46802 * r)), rounded (116130 * b))
{-# INLINE ycbcr #-}
