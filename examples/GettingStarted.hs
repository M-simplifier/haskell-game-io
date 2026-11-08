module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_, unless, void)
import qualified Data.List as List
import Data.Word (Word32)
import qualified GameIO.Audio as Audio
import qualified GameIO.Graphics as Graphics
import qualified GameIO.Input as Input
import qualified GameIO.Platform as Platform

data Model = Model
  { surface :: !(Int, Int)
  , marker :: !(Int, Int)
  , bright :: !Bool
  , finished :: !Bool
  }

main :: IO ()
main = Platform.withWindow config $ \window ->
  Graphics.withRenderer window $ \renderer ->
    Audio.withAudio Audio.defaultAudioConfig $ \audio -> do
      tone <- makeTone
      let playTone = void (Audio.playSound audio tone Audio.defaultVoiceOptions)
      size@(width, height) <- Platform.windowFramebufferSize window
      let initial = Model size (width `div` 2, height `div` 2) False False
      loop window renderer playTone Input.initialInput initial
  where
    config = Platform.WindowConfig "Game IO | Click or arrows to move | Space to play | Esc to close" 720 450

loop :: Platform.Window -> Graphics.Renderer -> IO () -> Input.InputState -> Model -> IO ()
loop window renderer playTone input model = do
  events <- Platform.pollWindowEventsStamped window
  let (input', changes) = Input.reduceInput input events
  let (next, tones) = List.foldl' update (model, 0) (map snd changes)
  unless (finished next) $ do
    replicateM_ tones playTone
    _ <- Graphics.renderFrame renderer (draw next)
    threadDelay 16000
    loop window renderer playTone input' next

-- Fold the ordered changes before drawing. A click and a later arrow press
-- produce a different position than the same two changes in reverse order.
update :: (Model, Int) -> Input.InputChange -> (Model, Int)
update (model, tones) change = case change of
  Input.InputClose -> (model {finished = True}, tones)
  Input.InputResize width height | width > 0 && height > 0 ->
    (place (marker model) (model {surface = (width, height)}), tones)
  Input.InputButton Input.PointerLeft True px py -> (place (px, py) model, tones)
  Input.InputKey Input.KeyPressed key -> press (Input.keyVirtual key)
  _ -> (model, tones)
  where
    (x, y) = marker model
    step = 16
    press key
      | key == escapeKey = (model {finished = True}, tones)
      | key == spaceKey = (model {bright = not (bright model)}, tones + 1)
      | key == leftKey = (place (x - step, y) model, tones)
      | key == rightKey = (place (x + step, y) model, tones)
      | key == upKey = (place (x, y - step) model, tones)
      | key == downKey = (place (x, y + step) model, tones)
      | otherwise = (model, tones)

place :: (Int, Int) -> Model -> Model
place (x, y) model = model {marker = (inside width x, inside height y)}
  where
    (width, height) = surface model
    inside extent coordinate
      | extent < 2 * radius = extent `div` 2
      | otherwise = max radius (min (extent - radius) coordinate)

draw :: Model -> Graphics.Frame
draw model = Graphics.Frame (Graphics.Color 0.04 0.07 0.12 1) []
  [Graphics.OverlayBatch Nothing [tl, tr, br, tl, br, bl]]
  where
    (x, y) = marker model
    tint = if bright model then Graphics.Color 0.99 0.58 0.22 1 else Graphics.Color 0.24 0.82 0.75 1
    vertex a b = Graphics.Vertex (fromIntegral a) (fromIntegral b) 0 0 0 tint
    tl = vertex (x - radius) (y - radius)
    tr = vertex (x + radius) (y - radius)
    br = vertex (x + radius) (y + radius)
    bl = vertex (x - radius) (y + radius)

makeTone :: IO Audio.PCM
makeTone = Audio.generatePCM sampleRate Audio.Mono frames $ \i ->
  let t = fromIntegral i / fromIntegral sampleRate
      edge = min 1 (fromIntegral (min i (frames - 1 - i)) / 220)
  in 0.12 * edge * sin (2 * pi * 523.25 * t)
  where
    sampleRate = 44100
    frames = sampleRate `div` 10

radius :: Int
radius = 22

-- Win32 virtual keys carried by the ordered input events.
escapeKey, spaceKey, leftKey, upKey, rightKey, downKey :: Word32
escapeKey = 0x1b
spaceKey = 0x20
leftKey = 0x25
upKey = 0x26
rightKey = 0x27
downKey = 0x28
