module GameIO.Text.Outline
  ( Point (..), Curve (..), Contour (..), Outline, addPoint, midpoint, mapOutline
  ) where

data Point = Point !Double !Double deriving (Eq, Show)
data Curve = Line !Point | Quadratic !Point !Point | Cubic !Point !Point !Point
  deriving (Eq, Show)
data Contour = Contour !Point ![Curve] deriving (Eq, Show)
type Outline = [Contour]

addPoint :: Point -> Point -> Point
addPoint (Point a b) (Point x y) = Point (a + x) (b + y)

midpoint :: Point -> Point -> Point
midpoint (Point a b) (Point x y) = Point ((a + x) * 0.5) ((b + y) * 0.5)

mapOutline :: (Point -> Point) -> Outline -> Outline
mapOutline f = map (\(Contour p curves) -> Contour (f p) (map transform curves))
  where
    transform (Line p) = Line (f p)
    transform (Quadratic p q) = Quadratic (f p) (f q)
    transform (Cubic p q r) = Cubic (f p) (f q) (f r)
