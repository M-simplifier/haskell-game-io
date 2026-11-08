module GameIO.Text.TrueType (TrueType, parseTrueType, trueTypeOutline) where

import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Bits (testBit)
import Data.ByteString qualified as BS
import Data.Sequence qualified as Seq
import GameIO.Text.Binary
import GameIO.Text.Outline

data TrueType = TrueType !(Array Int Int) !BS.ByteString

parseTrueType :: Int -> Int -> BS.ByteString -> BS.ByteString -> Either FontError TrueType
parseTrueType count format loca glyf = do
  offsets <- case format of
    0 -> traverse (fmap (* 2) . u16 loca . (* 2)) [0 .. count]
    1 -> traverse (u32 loca . (* 4)) [0 .. count]
    _ -> malformed "TrueType indexToLocFormat"
  ensure (and (zipWith (<=) offsets (drop 1 offsets)) && all (<= BS.length glyf) offsets) "TrueType glyph offsets"
  pure (TrueType (listArray (0, count) offsets) glyf)

data RawGlyph = RawGlyph !(Seq.Seq Point) !Outline

trueTypeOutline :: TrueType -> Int -> Either FontError Outline
trueTypeOutline font glyph = do
  (RawGlyph _ shape, _) <- readGlyph font [] 2048 glyph
  pure shape

readGlyph :: TrueType -> [Int] -> Int -> Int -> Either FontError (RawGlyph, Int)
readGlyph font@(TrueType offsets glyf) ancestors allowance glyph = do
  ensure (allowance > 0 && length ancestors < 16 && not (elem glyph ancestors)) "TrueType composite glyph cycle or limit"
  ensure (inRange (bounds offsets) glyph && inRange (bounds offsets) (glyph + 1)) "TrueType glyph index"
  let from = offsets ! glyph
      to = offsets ! (glyph + 1)
      empty = RawGlyph Seq.empty []
  if from == to then pure (empty, allowance - 1) else do
    bytes <- slice glyf from (to - from)
    _ <- slice bytes 0 10
    contours <- i16 bytes 0
    if contours >= 0
      then (, allowance - 1) <$> readSimple contours bytes
      else readComposite font (glyph : ancestors) bytes (allowance - 1)

readSimple :: Int -> BS.ByteString -> Either FontError RawGlyph
readSimple 0 _ = pure (RawGlyph Seq.empty [])
readSimple count bytes = do
  ends <- traverse (u16 bytes . (+ 10) . (* 2)) [0 .. count - 1]
  ensure (and (zipWith (<) ends (drop 1 ends))) "TrueType contour endpoints"
  total <- case reverse ends of
    end : _ -> pure (end + 1)
    [] -> malformed "TrueType contours"
  ensure (total <= 16384) "TrueType point limit"
  let instructions = 10 + 2 * count
  instructionCount <- u16 bytes instructions
  _ <- slice bytes (instructions + 2) instructionCount
  (flags, xOffset) <- readFlags bytes (instructions + 2 + instructionCount) total
  (xs, yOffset) <- readAxis bytes xOffset 1 4 flags
  (ys, _) <- readAxis bytes yOffset 2 5 flags
  let points = zipWith Point xs ys
      knots = zipWith (\flag -> Knot (testBit flag 0)) flags points
  shape <- traverse contour (splitContours 0 ends knots)
  pure (RawGlyph (Seq.fromList points) shape)

readFlags :: BS.ByteString -> Int -> Int -> Either FontError ([Int], Int)
readFlags bytes = go []
  where
    go acc offset remaining
      | remaining == 0 = Right (reverse acc, offset)
      | otherwise = do
          flag <- u8 bytes offset
          if testBit flag 3
            then do
              count <- (+ 1) <$> u8 bytes (offset + 1)
              ensure (count <= remaining) "TrueType repeated point flag exceeds point count"
              go (replicate count flag ++ acc) (offset + 2) (remaining - count)
            else go (flag : acc) (offset + 1) (remaining - 1)

readAxis :: BS.ByteString -> Int -> Int -> Int -> [Int] -> Either FontError ([Double], Int)
readAxis bytes start short same = go start 0 []
  where
    go offset _ acc [] = Right (reverse acc, offset)
    go offset here acc (flag : rest) = do
      (delta, next) <-
        if testBit flag short
        then do
          value <- u8 bytes offset
          pure ((if testBit flag same then value else negate value), offset + 1)
        else if testBit flag same then pure (0, offset)
             else (, offset + 2) <$> i16 bytes offset
      let point = here + delta
      go next point (fromIntegral point : acc) rest

data Knot = Knot !Bool !Point

splitContours :: Int -> [Int] -> [a] -> [[a]]
splitContours _ [] _ = []
splitContours from (end : ends) points =
  let (here, rest) = splitAt (end + 1 - from) points
  in here : splitContours (end + 1) ends rest

contour :: [Knot] -> Either FontError Contour
contour (Knot True first : rest) = Right (quadratics first rest)
contour knots@(Knot False first : _) =
  case reverse knots of
    Knot True lastPoint : _ -> Right (quadratics lastPoint knots)
    Knot False lastPoint : _ -> Right (quadratics (midpoint first lastPoint) knots)
    [] -> malformed "TrueType empty contour"
contour [] = malformed "TrueType empty contour"

quadratics :: Point -> [Knot] -> Contour
quadratics first knots = Contour first (go (knots ++ [Knot True first]))
  where
    go [] = []
    go (Knot True p : rest) = Line p : go rest
    go (Knot False control : Knot True p : rest) = Quadratic control p : go rest
    go (Knot False control : rest@(Knot False next : _)) = Quadratic control (midpoint control next) : go rest
    go [Knot False control] = [Quadratic control first]

readComposite :: TrueType -> [Int] -> BS.ByteString -> Int -> Either FontError (RawGlyph, Int)
readComposite font ancestors bytes allowance = go 10 allowance Seq.empty []
  where
    go offset remaining points contours = do
      flags <- u16 bytes offset
      child <- u16 bytes (offset + 2)
      ((a,b), transformOffset) <- readArguments bytes (offset + 4) flags
      (transform, next) <- readTransform bytes transformOffset flags
      (RawGlyph childPoints childContours, left) <- readGlyph font ancestors remaining child
      let transformedPoints = fmap transform childPoints
      translation <- if testBit flags 1
        then do
          ensure (not (testBit flags 11 && testBit flags 12)) "TrueType conflicting component offset modes"
          let offsetPoint = Point (fromIntegral a) (fromIntegral b)
          pure (if testBit flags 11 then transform offsetPoint else offsetPoint)
        else case (Seq.lookup a points, Seq.lookup b transformedPoints) of
          (Just (Point ax ay), Just (Point bx by)) -> Right (Point (ax - bx) (ay - by))
          _ -> malformed "TrueType invalid component attachment point"
      let joinedPoints = points Seq.>< fmap (addPoint translation) transformedPoints
          joinedContours = mapOutline (addPoint translation . transform) childContours : contours
      ensure (Seq.length joinedPoints <= 32768) "TrueType composite point limit"
      if testBit flags 5
        then go next left joinedPoints joinedContours
        else do
          if testBit flags 8 then do
            instructions <- u16 bytes next
            _ <- slice bytes (next + 2) instructions
            pure ()
          else pure ()
          pure (RawGlyph joinedPoints (concat (reverse joinedContours)), left)

readArguments :: BS.ByteString -> Int -> Int -> Either FontError ((Int, Int), Int)
readArguments bytes offset flags = do
  let words' = testBit flags 0
      isOffset = testBit flags 1
      width = if words' then 2 else 1
      readValue = case (words', isOffset) of
        (True, True) -> i16
        (True, False) -> u16
        (False, True) -> i8
        (False, False) -> u8
  a <- readValue bytes offset
  b <- readValue bytes (offset + width)
  pure ((a,b), offset + 2 * width)

readTransform :: BS.ByteString -> Int -> Int -> Either FontError (Point -> Point, Int)
readTransform bytes offset flags =
  case filter (testBit flags) [3, 6, 7] of
    [] -> Right (id, offset)
    [3] -> do
      scale <- f2dot14 offset
      pure (matrix scale 0 0 scale, offset + 2)
    [6] -> do
      x <- f2dot14 offset
      y <- f2dot14 (offset + 2)
      pure (matrix x 0 0 y, offset + 4)
    [7] -> do
      a <- f2dot14 offset
      b <- f2dot14 (offset + 2)
      c <- f2dot14 (offset + 4)
      d <- f2dot14 (offset + 6)
      pure (matrix a b c d, offset + 8)
    _ -> malformed "TrueType conflicting component transforms"
  where
    f2dot14 at = (/ 16384) . fromIntegral <$> i16 bytes at
    matrix a b c d (Point x y) = Point (a*x + c*y) (b*x + d*y)
