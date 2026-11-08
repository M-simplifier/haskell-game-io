{-# LANGUAGE OverloadedStrings #-}

module GameIO.Text
  ( Font
  , FontError (..)
  , decodeFontFace
  , fontUnitsPerEm
  , PixelSize
  , pixelSize
  , pixels
  , lineMetrics
  , GlyphId
  , notdefGlyph
  , glyphNumber
  , lookupGlyph
  , GlyphRun
  , PositionedGlyph
  , positionedGlyphId
  , glyphCluster
  , glyphX
  , glyphAdvance
  , layoutLine
  , layoutLineSpaced
  , runGlyphs
  , runAdvance
  , runInkBounds
  , caretAt
  , GlyphMask
  , maskBounds
  , maskAlpha
  , rasterGlyph
  ) where

import Control.Monad (foldM)
import Data.Array (Array, bounds, listArray, (!))
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import GameIO.Text.Binary
import GameIO.Text.CFF
import GameIO.Text.Outline (Outline)
import GameIO.Text.Raster
import GameIO.Text.TrueType

type Tables = Map.Map BS.ByteString BS.ByteString
data Outlines = QuadraticOutlines !TrueType | CubicOutlines !Cff
data CMap = SegmentMap !BS.ByteString !(Array Int Segment) | GroupMap !(Array Int Segment)
data Segment = Segment !Int !Int !Int !Int

data Font = Font !Int !Int !(Int, Int, Int) !Int !BS.ByteString !CMap !Outlines

decodeFontFace :: Int -> BS.ByteString -> Either FontError Font
decodeFontFace face bytes = do
  offset <- faceOffset face bytes
  scaler <- slice bytes offset 4
  ensure (elem scaler ["OTTO", "\x00\x01\x00\x00", "true"]) "Unsupported OpenType scaler type"
  tableCount <- u16 bytes (offset + 4)
  ensure (tableCount > 0 && tableCount <= 256) "OpenType table count"
  tables <- foldM (addTable bytes) Map.empty [offset + 12 + 16 * i | i <- [0 .. tableCount - 1]]
  headTable <- table "head" tables
  maxp <- table "maxp" tables
  hhea <- table "hhea" tables
  metrics <- table "hmtx" tables
  units <- u16 headTable 18
  count <- u16 maxp 4
  ensure (units >= 16 && units <= 16384 && count > 0) "OpenType units or glyph count"
  numberOfMetrics <- u16 hhea 34
  ensure (numberOfMetrics > 0 && numberOfMetrics <= count) "OpenType horizontal metric count"
  _ <- slice metrics 0 (4 * numberOfMetrics + 2 * (count - numberOfMetrics))
  vertical <- (,,) <$> i16 hhea 4 <*> i16 hhea 6 <*> i16 hhea 8
  cmap <- table "cmap" tables >>= readCmap
  outlines <- case Map.lookup "CFF " tables of
    Just cff -> CubicOutlines <$> parseCff count cff
    Nothing | Map.member "CFF2" tables -> Left (UnsupportedFont "CFF2 outlines")
    Nothing -> QuadraticOutlines <$> (parseTrueType count <$> i16 headTable 50 <*> table "loca" tables <*> table "glyf" tables >>= id)
  pure (Font units count vertical numberOfMetrics metrics cmap outlines)

faceOffset :: Int -> BS.ByteString -> Either FontError Int
faceOffset face bytes = do
  tag <- slice bytes 0 4
  if tag == "ttcf" then do
    count <- u32 bytes 8
    ensure (face >= 0 && face < count && face <= div (BS.length bytes) 4) "TrueType collection face index"
    u32 bytes (12 + 4 * face)
  else 0 <$ ensure (face == 0) "Standalone font face index"

addTable :: BS.ByteString -> Tables -> Int -> Either FontError Tables
addTable bytes tables offset = do
  name <- slice bytes offset 4
  start <- u32 bytes (offset + 8)
  size <- u32 bytes (offset + 12)
  ensure (not (Map.member name tables)) "Duplicate OpenType table"
  content <- slice bytes start size
  pure (Map.insert name content tables)

table :: BS.ByteString -> Tables -> Either FontError BS.ByteString
table name tables = maybe (malformed ("Missing OpenType table " ++ show name)) Right (Map.lookup name tables)

readCmap :: BS.ByteString -> Either FontError CMap
readCmap bytes = do
  count <- u16 bytes 2
  entries <- traverse directory [4 + i * 8 | i <- [0 .. count - 1]]
  case sortOn (Down . fst) [(rank, (offset, form)) | Just (rank, offset, form) <- entries] of
    (_, (offset, 12)) : _ -> readGroups offset
    (_, (offset, 4)) : _ -> readSegments offset
    _ -> Left (UnsupportedFont "No Unicode cmap format 4 or 12")
  where
    directory i = do
      platform <- u16 bytes i
      encoding <- u16 bytes (i + 2)
      offset <- u32 bytes (i + 4)
      if platform == 0 || platform == 3 && elem encoding [1,10] then do
        form <- u16 bytes offset
        let rank = if form == 12 then 2 :: Int else 1
        pure (if elem form [4,12] then Just (rank, offset, form) else Nothing)
      else pure Nothing
    readGroups offset = do
      size <- u32 bytes (offset + 4)
      body <- slice bytes offset size
      count <- u32 body 12
      ensure (count > 0 && count <= div (BS.length body - 16) 12) "cmap format 12 group count"
      ranges <- traverse (\i -> Segment <$> u32 body i <*> u32 body (i+4) <*> u32 body (i+8) <*> pure 0) [16 + 12*i | i <- [0 .. count-1]]
      validRanges 0x10ffff ranges
      pure (GroupMap (listArray (0, count - 1) ranges))
    readSegments offset = do
      size <- u16 bytes (offset + 2)
      body <- slice bytes offset size
      doubled <- u16 body 6
      let count = div doubled 2
      ensure (even doubled && count > 0 && 16 + 8 * count <= size) "cmap format 4 segment count"
      ranges <- traverse (readSegment body count) [0 .. count - 1]
      validRanges 0xffff ranges
      pure (SegmentMap body (listArray (0, count - 1) ranges))

readSegment :: BS.ByteString -> Int -> Int -> Either FontError Segment
readSegment body count index = do
  let offset = 2 * index
      rangeAddress = 16 + 6 * count + offset
  end <- u16 body (14 + offset)
  start <- u16 body (16 + 2 * count + offset)
  delta <- i16 body (16 + 4 * count + offset)
  extra <- u16 body rangeAddress
  let address = if extra == 0 then 0 else rangeAddress + extra
  ensure (extra == 0 || even extra && address >= 16 + 8 * count && address + 2 * (end - start) <= BS.length body - 2) "cmap glyph ID array offset"
  pure (Segment start end delta address)

validRanges :: Int -> [Segment] -> Either FontError ()
validRanges maximumCode ranges = do
  ensure (all (\(Segment first lastCode _ _) -> first <= lastCode && lastCode <= maximumCode) ranges) "cmap code point range"
  ensure (and (zipWith (\(Segment _ before _ _) (Segment after _ _ _) -> before < after) ranges (drop 1 ranges))) "cmap overlapping or unordered ranges"

findSegment :: Int -> Array Int Segment -> Maybe Segment
findSegment code ranges = uncurry find (bounds ranges)
  where
    find lo hi
      | lo > hi = Nothing
      | otherwise =
          let mid = div (lo + hi) 2
              range@(Segment first end _ _) = ranges ! mid
          in if code < first then find lo (mid - 1) else if code > end then find (mid + 1) hi else Just range

cmapIndex :: CMap -> Int -> Either FontError Int
cmapIndex (GroupMap ranges) code = pure $ case findSegment code ranges of
  Just (Segment first _ start _) -> start + (code - first)
  Nothing -> 0
cmapIndex (SegmentMap bytes ranges) code = case findSegment code ranges of
  Just (Segment first _ delta address)
    | address == 0 -> pure ((code + delta) .&. 0xffff)
    | otherwise -> do
        glyph <- u16 bytes (address + 2 * (code - first))
        pure (if glyph == 0 then 0 else (glyph + delta) .&. 0xffff)
  Nothing -> Right 0

fontUnitsPerEm :: Font -> Int
fontUnitsPerEm (Font units _ _ _ _ _ _) = units

newtype PixelSize = PixelSize Int deriving (Eq, Ord, Show)

pixelSize :: Int -> Maybe PixelSize
pixelSize size
  | size >= 4 && size <= 256 = Just (PixelSize size)
  | otherwise = Nothing

pixels :: PixelSize -> Int
pixels (PixelSize size) = size

fontScale :: Font -> PixelSize -> Double
fontScale font size = fromIntegral (pixels size) / fromIntegral (fontUnitsPerEm font)

lineMetrics :: Font -> PixelSize -> (Double, Double, Double)
lineMetrics font@(Font _ _ (ascender, descender, gap) _ _ _ _) size =
  (scale * fromIntegral ascender, scale * fromIntegral descender, scale * fromIntegral gap)
  where scale = fontScale font size

newtype GlyphId = GlyphId Int deriving (Eq, Ord, Show)

notdefGlyph :: GlyphId
notdefGlyph = GlyphId 0

glyphNumber :: GlyphId -> Int
glyphNumber (GlyphId value) = value

lookupGlyph :: Font -> Char -> Either FontError (Maybe GlyphId)
lookupGlyph (Font _ count _ _ _ cmap _) character = do
  index <- cmapIndex cmap (ord character)
  ensure (index < count) "cmap glyph ID outside font"
  pure (if index == 0 then Nothing else Just (GlyphId index))

data PositionedGlyph = PositionedGlyph !GlyphId !Int !Double !Double deriving (Eq, Show)
data GlyphRun = GlyphRun !Font !PixelSize ![PositionedGlyph] !Double

positionedGlyphId :: PositionedGlyph -> GlyphId
positionedGlyphId (PositionedGlyph identity _ _ _) = identity

glyphCluster :: PositionedGlyph -> Int
glyphCluster (PositionedGlyph _ cluster _ _) = cluster

glyphX :: PositionedGlyph -> Double
glyphX (PositionedGlyph _ _ x _) = x

glyphAdvance :: PositionedGlyph -> Double
glyphAdvance (PositionedGlyph _ _ _ advance) = advance

-- | A horizontal run for a UI label. Cluster indices address Unicode
-- scalars in the input string. The caller splits lines before layout.
layoutLine :: Font -> PixelSize -> String -> Either FontError GlyphRun
layoutLine font size = layoutLineSpaced font size 0

-- | Inter-glyph spacing is in pixels at this font size. It contributes to the
-- same caret positions, measurement, and draw geometry.
layoutLineSpaced :: Font -> PixelSize -> Double -> String -> Either FontError GlyphRun
layoutLineSpaced font@(Font _ _ _ metricCount metrics _ _) size spacing label = do
  ensure (spacing >= 0 && spacing <= 256) "Invalid glyph spacing"
  walk [] 0 0 label
  where
    scale = fontScale font size
    walk acc x _ [] = Right (GlyphRun font size (reverse acc) x)
    walk acc x cluster (character : rest) = do
      identity@(GlyphId index) <- maybe notdefGlyph id <$> lookupGlyph font character
      width <- (* scale) . fromIntegral <$> u16 metrics (4 * min (metricCount - 1) index)
      let advance = if null rest then width else width + spacing
      walk (PositionedGlyph identity cluster x advance : acc) (x + advance) (cluster + 1) rest

runGlyphs :: GlyphRun -> [PositionedGlyph]
runGlyphs (GlyphRun _ _ positions _) = positions

runAdvance :: GlyphRun -> Double
runAdvance (GlyphRun _ _ _ advance) = advance

caretAt :: GlyphRun -> Double -> Int
caretAt (GlyphRun _ _ positions _) x = go positions
  where
    go [] = 0
    go (PositionedGlyph _ cluster origin advance : rest)
      | x < origin + advance / 2 = cluster
      | null rest = cluster + 1
      | otherwise = go rest

runInkBounds :: GlyphRun -> Either FontError (Maybe (Double, Double, Double, Double))
runInkBounds (GlyphRun font size positions _) = foldM add Nothing positions
  where
    add previous (PositionedGlyph identity _ x _) = do
      mask <- rasterGlyph font size identity
      let (left, top, width, height) = maskBounds mask
          (l,t,r,b) = (x + fromIntegral left, fromIntegral top, x + fromIntegral (left + width), fromIntegral (top + height))
      pure $ if width == 0 || height == 0 then previous else Just $ case previous of
        Nothing -> (l,t,r,b)
        Just (a,c,d,e) -> (min a l, min c t, max d r, max e b)

rasterGlyph :: Font -> PixelSize -> GlyphId -> Either FontError GlyphMask
rasterGlyph font@(Font _ count _ _ _ _ outlines) size (GlyphId identity) = do
  ensure (identity >= 0 && identity < count) "Glyph ID outside font"
  shape <- glyphOutline outlines identity
  rasterize (fontScale font size) shape

glyphOutline :: Outlines -> Int -> Either FontError Outline
glyphOutline (QuadraticOutlines font) = trueTypeOutline font
glyphOutline (CubicOutlines font) = cffOutline font
