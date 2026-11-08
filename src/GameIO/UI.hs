module GameIO.UI
  ( PixelPoint (..)
  , Rect
  , rect
  , rectBounds
  , contains
  , intersectRect
  , ControlId (..)
  , Control (..)
  , Layout
  , LayoutError (..)
  , layout
  , controls
  , hitTest
  , Focus
  , initialFocus
  , focusedControl
  , UIEvent (..)
  , stepUI
  ) where

import Data.List (find)
import Data.Set qualified as Set

data PixelPoint = PixelPoint !Int !Int deriving (Eq, Show)

data Rect = Rect !Int !Int !Int !Int deriving (Eq, Show)

-- | Left and top are included. Right and bottom are excluded.
rect :: Int -> Int -> Int -> Int -> Maybe Rect
rect left top right bottom
  | left < right && top < bottom = Just (Rect left top right bottom)
  | otherwise = Nothing

rectBounds :: Rect -> (Int, Int, Int, Int)
rectBounds (Rect left top right bottom) = (left, top, right, bottom)

contains :: Rect -> PixelPoint -> Bool
contains (Rect left top right bottom) (PixelPoint x y) =
  left <= x && x < right && top <= y && y < bottom

intersectRect :: Rect -> Rect -> Maybe Rect
intersectRect (Rect l t r b) (Rect l' t' r' b') =
  rect (max l l') (max t t') (min r r') (min b b')

newtype ControlId = ControlId Int deriving (Eq, Ord, Show)

data Control = Control
  { controlId :: !ControlId
  , controlBounds :: !Rect
  , controlEnabled :: !Bool
  } deriving (Eq, Show)

newtype Layout = Layout [Control] deriving (Eq, Show)

data LayoutError = RepeatedControl !ControlId deriving (Eq, Show)

-- | Controls are listed in paint order. Their enabled subset also defines
-- the traversal order for keyboard focus.
layout :: [Control] -> Either LayoutError Layout
layout items = Layout items <$ check Set.empty items
  where
    check _ [] = Right ()
    check seen (item : rest)
      | Set.member (controlId item) seen = Left (RepeatedControl (controlId item))
      | otherwise = check (Set.insert (controlId item) seen) rest

controls :: Layout -> [Control]
controls (Layout items) = items

hitTest :: Layout -> PixelPoint -> Maybe ControlId
hitTest (Layout items) point =
  controlId <$> find (\item -> controlEnabled item && contains (controlBounds item) point) (reverse items)

newtype Focus = Focus (Maybe ControlId) deriving (Eq, Show)

initialFocus :: Focus
initialFocus = Focus Nothing

focusedControl :: Focus -> Maybe ControlId
focusedControl (Focus identity) = identity

data UIEvent = FocusNext | FocusPrevious | PointerActivate !PixelPoint | ActivateFocus | ClearFocus
  deriving (Eq, Show)

-- | An activation is delivered separately from focus. Layout replacement
-- invalidates focus on controls that disappeared or became disabled.
stepUI :: Layout -> UIEvent -> Focus -> (Focus, Maybe ControlId)
stepUI view event (Focus previous) =
  case event of
    FocusNext -> (Focus (next enabled), Nothing)
    FocusPrevious -> (Focus (next (reverse enabled)), Nothing)
    PointerActivate point ->
      let hit = hitTest view point
      in (Focus hit, hit)
    ActivateFocus -> (Focus current, current)
    ClearFocus -> (initialFocus, Nothing)
  where
    enabled = map controlId (filter controlEnabled (controls view))
    current = previous >>= \identity -> find (== identity) enabled
    next order = case order of
      [] -> Nothing
      first : _ -> case current >>= after order of
        Just successor -> Just successor
        Nothing -> Just first
    after order identity = case dropWhile (/= identity) order of
      _ : successor : _ -> Just successor
      _ -> Nothing
