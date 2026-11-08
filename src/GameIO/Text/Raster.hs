module GameIO.Text.Raster (GlyphMask, maskBounds, maskAlpha, rasterize) where

import Data.ByteString qualified as BS
import Data.Foldable qualified as Foldable
import Data.List (sortOn)
import Data.Word (Word8)
import GameIO.Text.Binary
import GameIO.Text.Outline

-- | Left and top are offsets from the glyph origin and baseline, in pixels.
data GlyphMask = GlyphMask !Int !Int !Int !Int !BS.ByteString
  deriving (Eq, Show)

maskBounds :: GlyphMask -> (Int, Int, Int, Int)
maskBounds (GlyphMask l t w h _) = (l, t, w, h)

maskAlpha :: GlyphMask -> BS.ByteString
maskAlpha (GlyphMask _ _ _ _ alpha) = alpha

data Edge = Edge !Point !Point

rasterize :: Double -> Outline -> Either FontError GlyphMask
rasterize scale shape = case edges of
  [] -> pure (GlyphMask 0 0 0 0 BS.empty)
  Edge initial _ : _ -> do
    ensure (length edges <= 16384) "Rasterized glyph outline is too complex"
    let (l,t,r,b) = Foldable.foldl' includeEdge (pointBox initial) edges
    ensure (all (\v -> abs v <= 1000000) [l,t,r,b]) "Glyph coordinates out of range"
    let left = floor l
        top = floor t
        width = ceiling r - left
        height = ceiling b - top
    if width > 2048 || height > 2048 || width * height > 1048576
      then Left (GlyphTooLarge width height)
      else pure (GlyphMask left top width height (BS.pack (concatMap (coverageRow edges left width) [top .. top + height - 1])))
  where
    edges = take 16385 (concatMap flatten (mapOutline toScreen shape))
    toScreen (Point x y) = Point (x * scale) (-y * scale)

pointBox :: Point -> (Double, Double, Double, Double)
pointBox (Point x y) = (x, y, x, y)

includeEdge :: (Double, Double, Double, Double) -> Edge -> (Double, Double, Double, Double)
includeEdge box (Edge a b) = include (include box a) b
  where
    include (l,t,r,bottom) (Point x y) = (min l x, min t y, max r x, max bottom y)

flatten :: Contour -> [Edge]
flatten (Contour first curves) = go first curves
  where
    go lastPoint [] = [Edge lastPoint first]
    go here (Line p : rest) = Edge here p : go p rest
    go here (Quadratic p q : rest) = quadratic 0 here p q (go q rest)
    go here (Cubic p q r : rest) = cubic 0 here p q r (go r rest)

curvature :: Point -> Point -> Point -> Double
curvature (Point ax ay) (Point bx by) (Point cx cy) =
  max (abs (ax - 2 * bx + cx)) (abs (ay - 2 * by + cy))

quadratic :: Int -> Point -> Point -> Point -> [Edge] -> [Edge]
quadratic depth a b c rest
  | depth >= 10 || curvature a b c <= 0.20 = Edge a c : rest
  | otherwise =
      let ab = midpoint a b
          bc = midpoint b c
          centre = midpoint ab bc
      in quadratic (depth + 1) a ab centre (quadratic (depth + 1) centre bc c rest)

cubic :: Int -> Point -> Point -> Point -> Point -> [Edge] -> [Edge]
cubic depth a b c d rest
  | depth >= 10 || max (curvature a b c) (curvature b c d) <= 0.20 = Edge a d : rest
  | otherwise =
      let ab = midpoint a b
          bc = midpoint b c
          cd = midpoint c d
          p = midpoint ab bc
          q = midpoint bc cd
          centre = midpoint p q
      in cubic (depth + 1) a ab p centre (cubic (depth + 1) centre q cd d rest)

coverageRow :: [Edge] -> Int -> Int -> Int -> [Word8]
coverageRow edges left width row = map (\samples -> fromIntegral (div (samples * 255 + 8) 16)) coverage
  where
    coverage :: [Int]
    coverage = Foldable.foldl' (zipWith (+)) (replicate width 0) [scan (fromIntegral row + dy) | dy <- [0.125, 0.375, 0.625, 0.875]]
    scan y = groups (sweep (sortOn fst (foldr (crossing y) [] edges)) 0 sampleX)
    sampleX = [fromIntegral left + (fromIntegral x + 0.5) / 4 | x <- [0 .. width * 4 - 1]]
    groups (a:b:c:d:rest) = a+b+c+d : groups rest
    groups _ = []
    sweep _ _ [] = []
    sweep ((edgeX, sign) : later) winding samples@(x : _)
      | edgeX <= x = sweep later (winding + sign) samples
    sweep pending winding (_ : xs) = (if winding == 0 then 0 else 1) : sweep pending winding xs

crossing :: Double -> Edge -> [(Double, Int)] -> [(Double, Int)]
crossing y (Edge (Point ax ay) (Point bx by)) rest
  | ay <= y && y < by = (x, 1) : rest
  | by <= y && y < ay = (x, -1) : rest
  | otherwise = rest
  where
    x = ax + (bx - ax) * (y - ay) / (by - ay)
