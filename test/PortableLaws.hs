module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import GameIO.Image
import GameIO.Input
import GameIO.UI

main :: IO ()
main = do
  uiChecks
  inputChecks
  imageChecks
  putStrLn "Portable UI, input, and image checks passed."

uiChecks :: IO ()
uiChecks = do
  panel <- maybe (fail "invalid panel fixture") pure (rect 0 0 100 100)
  button <- maybe (fail "invalid button fixture") pure (rect 25 25 50 50)
  let first = ControlId 1
      second = ControlId 2
      item = Control first panel True
  view <- either (fail . show) pure (layout [item, Control second button True])
  check "the top painted button receives the hit" (hitTest view (PixelPoint 25 25) == Just second)
  check "the right edge is excluded" (hitTest view (PixelPoint 50 25) == Just first)
  check "repeated IDs are rejected" (layout [item, item] == Left (RepeatedControl first))
  let (one, _) = stepUI view FocusNext initialFocus
      (two, _) = stepUI view FocusNext one
      (again, _) = stepUI view FocusNext two
  check "focus traverses both enabled controls and wraps" (map focusedControl [one, two, again] == map Just [first, second, first])
  replacement <- either (fail . show) pure (layout [item])
  check "removed focus cannot activate" (stepUI replacement ActivateFocus two == (initialFocus, Nothing))

inputChecks :: IO ()
inputChecks = do
  let down = KeyMessage 65 30 False True False
      up = down {keyDown = False}
      repeated = down {keyRepeated = True}
      focus = TimedWindowEvent 0 0 (WindowFocusChanged True)
      first = TimedWindowEvent 3000000 1 (WindowKeyChanged down)
      second = TimedWindowEvent 4000000 2 (WindowKeyChanged up)
      later = TimedWindowEvent 20000000 3 (WindowKeyChanged down)
      events = [focus, first, second, later]
  complete <- success (acceptEvents events initialTimeline)
  split <- success (acceptEvents [focus, first] initialTimeline >>= acceptEvents [second, later])
  check "delivery partition preserves the accepted sequence" (split == complete)
  (tick, next) <- success (takeThrough 16000000 complete)
  let (after, changes) = reduceInput initialInput tick
  check "3ms press and 4ms release survive the 16ms boundary"
    (map snd changes == [InputFocus True, InputKey KeyPressed down, InputKey KeyReleased up] && not (keyHeld after down))
  (nextTick, closed) <- success (takeThrough 32000000 next)
  check "a later tick sees the edge once" (nextTick == [later])
  check "late input is reported to the caller" (acceptEvents [TimedWindowEvent 31000000 4 CloseRequested] closed == Left (EventAfterClosedTick 32000000 (HostStamp 31000000 4)))
  let raw = [ focus
            , first
            , TimedWindowEvent 3000000 2 (WindowKeyChanged repeated)
            , TimedWindowEvent 3000000 3 (WindowComposition "あ")
            , TimedWindowEvent 3100000 4 (WindowTextCodeUnit 0xd842)
            , TimedWindowEvent 3200000 5 (WindowTextCodeUnit 0xdf9f)
            , TimedWindowEvent 3300000 6 (WindowFocusChanged False)
            ]
  tied <- success (acceptEvents raw initialTimeline)
  (within, _) <- success (takeThrough 3300000 tied)
  let (lost, messages) = reduceInput initialInput within
  check "native repeat, composition, committed text, and cancellation stay distinct"
    (map snd messages == [ InputFocus True, InputKey KeyPressed down, InputKey KeyAutoRepeat repeated
                         , InputComposition "あ", InputText '\x20b9f', InputFocus False, InputCancel FocusLost
                         ] && not (keyHeld lost down || hasFocus lost))

success :: (Show e) => Either e a -> IO a
success = either (fail . show) pure

imageChecks :: IO ()
imageChecks = do
  -- Independent zlib fixture, RGB, Sub then Up scanline filters.
  let fixture = BS.pack
        [137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,2,0,0,0,2
        ,8,2,0,0,0,253,212,154,115,0,0,0,21,73,68,65,84,120,1,99,252,207
        ,192,192,240,159,129,137,17,72,48,252,7,0,27,21,4,1,140,183,183
        ,25,0,0,0,0,73,69,78,68,174,66,96,130]
      expected = BS.pack [255,0,0,255,255,255,0,255,0,0,255,255,255,255,255,255]
  loaded <- success (decodeImage fixture)
  check "PNG matches the fixed independent pixel witness" (imageSize loaded == (2,2) && imageRGBA loaded == expected)
  transparent <- success (rgbaImage 1 2 (BS.pack [10,20,30,40,50,60,70,80]))
  reloaded <- success (decodeImage (encodePng transparent))
  check "PNG capture keeps straight alpha and row order" (imageRGBA reloaded == imageRGBA transparent && imageSize reloaded == (1,2))
  check "PNG rejects a corrupt IDAT checksum"
    (case decodeImage (BS.take 48 fixture <> BS.singleton 0 <> BS.drop 49 fixture) of
       Left (InvalidImage _) -> True
       _ -> False)

check :: String -> Bool -> IO ()
check description passed = unless passed (fail description)
