module GameIO.Text.Atlas
  ( GlyphAtlas
  , AtlasError (..)
  , TextQuad (..)
  , buildAtlas
  , atlasSize
  , atlasRGBA
  , atlasQuads
  , atlasLineMetrics
  , atlasLine
  , atlasQuadsSpaced
  ) where

import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (pokeByteOff)
import GameIO.Text

data Region = Region !Int !Int !Int !Int !Int !Int
data GlyphAtlas = GlyphAtlas !Font !PixelSize !Int !Int !BS.ByteString !(Map.Map GlyphId (Maybe Region))

data AtlasError
  = InvalidAtlasWidth !Int
  | AtlasFull !Int !Int
  | MissingAtlasGlyph !GlyphId
  | AtlasFontError !FontError
  deriving (Eq, Show)

-- | Pixel-space rectangle and normalized UV rectangle, both left/top/right/bottom.
data TextQuad = TextQuad !(Double, Double, Double, Double) !(Float, Float, Float, Float)
  deriving (Eq, Show)

data Packing = Packing !Int !Int !Int !(Map.Map GlyphId (Maybe Region)) !(IntMap.IntMap [(Int, BS.ByteString)])

-- | A complete immutable texture. A newly required glyph creates a new atlas
-- without modifying the texture sampled by already submitted frames.
buildAtlas :: Int -> Font -> PixelSize -> String -> Either AtlasError GlyphAtlas
buildAtlas width font size characters
  | width < 32 || width > 4096 = Left (InvalidAtlasWidth width)
  | otherwise = do
      catalogue <- fontError (layoutLine font size (Set.toList (Set.fromList characters)))
      let ids = Set.toList (Set.insert notdefGlyph (Set.fromList (map positionedGlyphId (runGlyphs catalogue))))
      masks <- traverse (\identity -> (identity,) <$> fontError (rasterGlyph font size identity)) ids
      let byHeight = sortOn (\(_, mask) -> let (_,_,_,h) = maskBounds mask in Down h) masks
      Packing _ y rowHeight regions rows <- foldM (pack width) (Packing 1 1 0 Map.empty IntMap.empty) byHeight
      let height = y + rowHeight + 1
          alpha = BS.concat [makeRow width (IntMap.findWithDefault [] row rows) | row <- [0 .. height - 1]]
      pure (GlyphAtlas font size width height (whiteAlpha alpha) regions)

pack :: Int -> Packing -> (GlyphId, GlyphMask) -> Either AtlasError Packing
pack width (Packing x y rowHeight regions rows) (identity, mask)
  | w == 0 || h == 0 = Right (Packing x y rowHeight (Map.insert identity Nothing regions) rows)
  | w + 2 > width || y' + h + 1 > 4096 = Left (AtlasFull width (y' + h + 1))
  | otherwise =
      let region = Region x' y' left top w h
          rowEntries = [(y' + row, [(x', BS.take w (BS.drop (row * w) alpha))]) | row <- [0 .. h - 1]]
          accumulatedRows = IntMap.unionWith (++) rows (IntMap.fromDistinctAscList rowEntries)
      in Right (Packing (x' + w + 1) y' (max height' h) (Map.insert identity (Just region) regions) accumulatedRows)
  where
    (left, top, w, h) = maskBounds mask
    alpha = maskAlpha mask
    (x', y', height')
      | x + w + 1 <= width = (x, y, rowHeight)
      | otherwise = (1, y + rowHeight + 1, 0)

makeRow :: Int -> [(Int, BS.ByteString)] -> BS.ByteString
makeRow width pieces = BS.concat (go 0 (sortOn fst pieces))
  where
    go end [] = [BS.replicate (width - end) 0]
    go end ((x, bytes) : rest) = BS.replicate (x - end) 0 : bytes : go (x + BS.length bytes) rest

-- The allocation belongs to this ByteString. Only its local alpha bytes are
-- changed before the pointer is returned to bytestring's allocator.
whiteAlpha :: BS.ByteString -> BS.ByteString
whiteAlpha alpha = BSI.unsafeCreate (4 * size) $ \destination -> do
  fillBytes destination 255 (4 * size)
  let write index
        | index >= size = pure ()
        | otherwise = do
            pokeByteOff destination (4 * index + 3) (BS.index alpha index)
            write (index + 1)
  write 0
  where size = BS.length alpha

atlasSize :: GlyphAtlas -> (Int, Int)
atlasSize (GlyphAtlas _ _ width height _ _) = (width, height)

atlasRGBA :: GlyphAtlas -> BS.ByteString
atlasRGBA (GlyphAtlas _ _ _ _ rgba _) = rgba

atlasLineMetrics :: GlyphAtlas -> (Double, Double, Double)
atlasLineMetrics (GlyphAtlas font size _ _ _ _) = lineMetrics font size

atlasLine :: GlyphAtlas -> Double -> String -> Either AtlasError GlyphRun
atlasLine (GlyphAtlas font size _ _ _ _) spacing label = fontError (layoutLineSpaced font size spacing label)

atlasQuads :: GlyphAtlas -> Double -> Double -> String -> Either AtlasError (GlyphRun, [TextQuad])
atlasQuads atlas left baseline = atlasQuadsSpaced atlas left baseline 0

atlasQuadsSpaced :: GlyphAtlas -> Double -> Double -> Double -> String -> Either AtlasError (GlyphRun, [TextQuad])
atlasQuadsSpaced atlas@(GlyphAtlas _ _ width height _ regions) left baseline spacing label = do
  run <- atlasLine atlas spacing label
  quads <- foldM draw [] (runGlyphs run)
  pure (run, reverse quads)
  where
    draw acc placement = case Map.lookup (positionedGlyphId placement) regions of
      Nothing -> Left (MissingAtlasGlyph (positionedGlyphId placement))
      Just Nothing -> pure acc
      Just (Just (Region x y bearingX bearingY w h)) ->
        let l = left + glyphX placement + fromIntegral bearingX
            t = baseline + fromIntegral bearingY
            r = l + fromIntegral w
            b = t + fromIntegral h
        in pure (TextQuad (l,t,r,b) (u x, v y, u (x+w), v (y+h)) : acc)
    u x = fromIntegral x / fromIntegral width
    v y = fromIntegral y / fromIntegral height

fontError :: Either FontError a -> Either AtlasError a
fontError = either (Left . AtlasFontError) Right
