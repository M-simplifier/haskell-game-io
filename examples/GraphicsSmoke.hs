{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sort)
import Data.Word (Word8, Word64)
import GameIO.Graphics
import GameIO.Graphics.Shader
import GameIO.Platform
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [] -> run True Nothing 0
    ["--hidden"] -> run False Nothing 0
    ["--ppm", path] -> run True (Just path) 0
    ["--emit-shaders", directory] -> emitShaders directory
    ["--bench", value] | Just frames <- readMaybe value, frames > 0 -> run True Nothing frames
    _ -> fail "Usage: graphics-smoke [--hidden | --ppm PATH | --bench FRAME_COUNT | --emit-shaders DIR]"

run :: Bool -> Maybe FilePath -> Int -> IO ()
run visible output benchFrames = do
  checkProjection
  withWindowVisibility visible (WindowConfig "Haskell Game IO Graphics Smoke" 320 200) $ \window ->
    withRenderer window $ \renderer -> do
      putStrLn ("adapter " ++ rendererAdapterName renderer)
      nearData <- either (fail . show) pure (triangle 1 (Color 0.15 0.68 0.31 1))
      farData <- either (fail . show) pure (triangle 2 (Color 0.30 0.19 0.82 1))
      bracket (createMesh renderer nearData) (destroyMesh renderer) $ \near ->
        bracket (createMesh renderer farData) (destroyMesh renderer) $ \far -> do
          texture <- createTextureRGBA8 renderer 2 2 checker
          let makeFrame :: Int -> Int -> IO Frame
              makeFrame width height = do
                projection <- either (fail . show) pure $
                  perspectiveMat4 (pi / 2) (fromIntegral width / fromIntegral height) 0.25 20
                camera <- either (fail . show) pure $
                  lookAtMat4 (Vec3 0 0 0) (Vec3 0 0 (-1)) (Vec3 0 1 0)
                let viewProjection = multiplyMat4 projection camera
                let model x y = multiplyMat4 viewProjection (translateMat4 (Vec3 x y 0))
                pure $ Frame background
                  [WorldDraw near Nothing (model 0.16 0.11), WorldDraw far Nothing (model 0.32 0.22)]
                  [ OverlayBatch (Just texture) (quad 14 18 64 48 white)
                  , ClippedOverlay (ClipRect (width - 44) 12 17 36) Nothing
                      (quad (fromIntegral (width - 50)) 12 42 36 (Color 0.85 0.15 0.05 0.5))
                  ]
          initial <- checkFrame window renderer makeFrame
          mapM_ (\path -> writePPM path initial) output
          putStrLn ("verified color, depth, texture, alpha and clip " ++ show (pixelWidth initial, pixelHeight initial))
          resizeWindow window 420 230
          resized <- checkFrame window renderer makeFrame
          unless ((pixelWidth resized, pixelHeight resized) == (420, 230)) $
            fail ("Unexpected framebuffer after resize " ++ show (pixelWidth resized, pixelHeight resized))
          putStrLn "verified swapchain and depth recreation 420x230"
          checkOffscreen window renderer
          checkMaterials window renderer
          checkShaderSemantics window renderer
          checkInstancedDerivative window renderer
          when (benchFrames > 0) $ do
            frame <- makeFrame 420 230
            let largeOverlay = OverlayBatch (Just texture) (concatMap tile [0..999])
            benchmark benchFrames window renderer (frame { frameOverlay = frameOverlay frame ++ [largeOverlay] })
          destroyTexture renderer texture
          obsolete <- makeFrame 420 230
          released <- try (renderFrame renderer obsolete)
          unless (released == Left ResourceReleased) $
            fail ("Released texture remained usable " ++ show released)
          putStrLn "verified released texture rejected"

checkProjection :: IO ()
checkProjection = do
  projection <- either (fail . show) pure (perspectiveMat4 (pi / 2) 1.7 0.25 20)
  let actual x y d = case transformVec4 projection (Vec4 x y (-d) 1) of
        Vec4 a b c w -> [a/w, b/w, c/w]
  let reference x y distance =
        [x / (1.7 * distance), -y / distance, 20 * (distance - 0.25) / (distance * (20 - 0.25))]
  forM_ [(0.11, -0.17, 0.25), (0.24, 0.07, 2.7), (-1.1, -0.8, 20)] $ \(x,y,d) ->
    unless (and (zipWith (\a b -> abs (a - b) < 1e-5) (actual x y d) (reference x y d))) $
      fail ("perspective does not map Vulkan clip space at " ++ show (x,y,d))

checkMaterials :: Window -> Renderer -> IO ()
checkMaterials window renderer = do
  parameter <- maybe (fail "shader parameter slot unavailable") pure (uniformSlot 2)
  bound <- maybe (fail "shader loop bound unavailable") pure (loopLimit 4)
  let config = MaterialConfig OverlayReplace defaultVertexShader (diagnosticShader parameter bound)
  bracket (createMaterial renderer config) (destroyMaterial renderer) $ \material ->
    bracket (createTextureRGBA8 renderer 1 1 (BS.pack [120, 80, 40, 255])) (destroyTexture renderer) $ \first ->
      bracket (createTextureRGBA8 renderer 1 1 (BS.pack [20, 60, 100, 255])) (destroyTexture renderer) $ \second -> do
        let draw x factors = OverlayBatchWith Nothing material
              (ShaderInputs [(Texture1, first), (Texture3, second)]
                [Vec4 0.97 0.92 0.81 1, Vec4 0.62 0.22 0.07 1, factors])
              (quad x 11 40 32 white)
        pixels <- readPresented window renderer []
          (Frame background [] [draw 10 (Vec4 0.35 0.60 0.80 0.75), draw 70 (Vec4 0.15 0.25 0.50 0.75)])
        expect pixels "two samplers and asymmetric uniform values" 23 20 [47, 63, 57, 255]
        expect pixels "second draw uses a different aligned block" 85 20 [23, 35, 45, 255]
        putStrLn "verified SPIR-V material, distinct samplers, std140 slot and per-draw dynamic offset"

diagnosticShader :: UniformSlot -> LoopLimit -> FragmentShader
diagnosticShader slot bound = FragmentShader $
  letExpr (Uniform slot) $ \factors ->
    let weighted = Multiply (SampleAt Texture1 InterpolatedUV) factors
        amount = foldCount Float1 bound (CountFromFloat (FloatLiteral 2)) (FloatLiteral 0) $ \_ total ->
          FloatBinary Float1 AddF total (FloatUnary Float1 Absolute (FloatLiteral (-0.125)))
        added = ScaleVector Vector4 (SampleAt Texture3 InterpolatedUV)
          amount
        pass = Compare Less (Component W4 factors) (FloatLiteral 1)
    in If Float4 pass (FloatBinary Float4 AddF weighted added) (Constant4 (Vec4 1 0 1 1))

checkShaderSemantics :: Window -> Renderer -> IO ()
checkShaderSemantics window renderer = do
  parameter <- maybe (fail "shader parameter slot unavailable") pure (uniformSlot 2)
  bound <- maybe (fail "shader loop bound unavailable") pure (loopLimit 4)
  let fragment = bindingProbe bound
  let point = FragmentSample (Vec2 0 0) white (Vec3 0 0 0) (Vec3 0 1 0)
        (Vec3 0 0 0) (Vec3 0 1 0) (Vec2 0 0) (Vec4 0 0 0 1) True
  let cpuColor = evaluateFragment fragment (\_ _ -> white) (const (Vec4 0 0 0 0)) (FragmentQuad point point point point)
  unless (cpuColor == Just (Color 0.25 0.375 0.625 1)) $
    fail ("shader binding differed from the lexical reference: " ++ show cpuColor)
  let factors = Vec4 0.125 0.25 0.375 0.5
  let otherFactors = Vec4 0.9 0.8 0.7 0.6
  let parameters slot | slot == parameter = factors
                      | otherwise = otherFactors
  let sourceUV = Vec2 0.0625 0.125
  let sourceColor = Color 0.5 0.75 0.25 0.875
  let sourceNormal = Vec3 0.625 0.25 0.75
  let vertexAt p = VertexWithNormal p sourceUV sourceColor sourceNormal
  let matrix = mat4FromColumns (Vec4 1.25 (-0.5) 0.25 0) (Vec4 0.25 2 0.125 0)
        (Vec4 (-0.25) 0.5 1.5 0) (Vec4 0.75 (-0.125) 0.375 1)
  let vertex = vertexProbe parameter
  let cpuVertex = evaluateVertexWith vertex parameters matrix (vertexAt (Vec3 0.25 0.5 0.75))
  let expected = VertexResult (Vec4 1 1.125 1.625 1) (Vec2 0.3125 0.5)
        (Color 0.0625 0.1875 0.09375 0.4375) (Vec3 0.375 1.125 (-0.25)) (Vec3 0.3125 0.125 0.375)
        (Vec3 0.25 0.5 0.75) sourceNormal (Vec2 0 0)
  unless (cpuVertex == expected) (fail ("vertex interpreter differed from the numeric reference: " ++ show cpuVertex))
  let bindingMaterial = MaterialConfig OverlayReplace defaultVertexShader fragment
  let vertexMaterial = MaterialConfig OverlayReplace vertex varyingProbe
  bracket (createMaterial renderer bindingMaterial) (destroyMaterial renderer) $ \binding ->
    bracket (createMaterial renderer vertexMaterial) (destroyMaterial renderer) $ \varyings -> do
      let drawBindings = OverlayBatchWith Nothing binding (ShaderInputs [] []) (quad 10 11 40 32 white)
      let replaceVertex (Vertex x y _ _ _ _) = vertexAt (Vec3 x y 0.25)
          replaceVertex v = v
      let vertices = map replaceVertex (quad 70 11 40 32 white)
      let drawVaryings = OverlayBatchWith Nothing varyings
            (ShaderInputs [] [otherFactors, otherFactors, factors]) vertices
      pixels <- readPresented window renderer [] (Frame background [] [drawBindings, drawVaryings])
      expect pixels "opaque let and loop bindings match the CPU reference" 23 20 [64, 96, 159, 255]
      expect pixels "vertex stage uniform and world varyings reach fragment stage" 85 20 [80, 159, 96, 112]
  putStrLn "verified opaque let and loop bindings, vertex uniforms and all CPU vertex outputs"

bindingProbe :: LoopLimit -> FragmentShader
bindingProbe bound = FragmentShader $
  letExpr (FloatLiteral 0.25) $ \local ->
    let folded = foldCount Float1 bound (UnsignedLiteral 2) 0.125 $ \index total ->
          case (index, total) of
            (UnsignedLiteral _, _) -> 0.9
            (_, FloatLiteral _) -> 0.85
            _ -> total + UnsignedFloat index / 4
    in case local of
      FloatLiteral _ -> Constant4 (Vec4 1 0 0 1)
      _ -> MakeVec4 local folded 0.625 1

vertexProbe :: UniformSlot -> VertexShader
vertexProbe parameter = WorldVertexShader
  (Transform (Homogeneous VertexPosition))
  (letExpr (Uniform parameter) $ \u ->
    MakeVec2 (Component X2 VertexUV + Component Y4 u) (Component Y2 VertexUV + Component Z4 u))
  (Multiply VertexColor (Uniform parameter))
  (letExpr (Uniform parameter) $ \u ->
    MakeVec3 (Component X3 VertexPosition + Component X4 u)
      (Component Z3 VertexPosition + Component Z4 u) (Component Y4 u - Component Y3 VertexPosition))
  (ScaleVector Vector3 VertexNormal (Component W4 (Uniform parameter)))

varyingProbe :: FragmentShader
varyingProbe = FragmentShader $ MakeVec4 (Component X2 InterpolatedUV) (Component Y3 InterpolatedPosition)
  (Component Z3 InterpolatedNormal) (Component W4 InterpolatedColor)

checkInstancedDerivative :: Window -> Renderer -> IO ()
checkInstancedDerivative window renderer = do
  let fragment = instancedFragment
  let point x y = FragmentSample (Vec2 0.1875 0.375) (Color 0.25 0.5 0.7 1) (Vec3 0 0 0) (Vec3 0 1 0)
        (Vec3 0 0 0) (Vec3 0 1 0) (Vec2 0 0) (Vec4 x y 0.35 1) True
  let quadAt x y = FragmentQuad (point x y) (point (x+1) y) (point x (y+1)) (point (x+1) (y+1))
  let evaluate = evaluateFragment fragment (\_ _ -> white) (const (Vec4 0 0 0 0))
  unless (evaluate (quadAt 128.5 113.5) == Just (Color 0.3125 0.625 0.7 1) &&
      evaluate (quadAt 116.5 113.5) == Nothing) $
    fail "shader derivative or discard disagreed with the 2x2 reference"
  let corners = [Vec3 (-0.24) (-0.20) 0.35, Vec3 0.24 (-0.20) 0.35, Vec3 0 0.32 0.35]
  let records = [ InstanceData (Vec4 (-0.40) 0.05 0 0.75) (Vec4 0.375 0.9 0.4 0.1) (19, 43, 179, 255) 342
                , InstanceData (Vec4 0.40 (-0.06) 0 0.5) (Vec4 0.125 0.8 0.3 0.2) (213, 89, 78, 255) 62 ]
  let pixel = Lazy.toStrict (Builder.toLazyByteString (Builder.floatLE 0.1875))
  let program = MaterialConfig WorldDepth instancedVertex fragment
  bracket (createInstanceTemplate renderer corners) (destroyInstanceTemplate renderer) $ \template ->
    bracket (createInstances renderer records) (destroyInstances renderer) $ \instances ->
      bracket (createTextureFloat32 renderer Nearest R32Float 1 1 pixel) (destroyTexture renderer) $ \height ->
        bracket (createMaterial renderer program) (destroyMaterial renderer) $ \material -> do
          let draw = WorldInstances template instances material (ShaderInputs [(Texture2, height)] []) identityMat4
          pixels <- readPresented window renderer [] (Frame background [draw] [])
          expect pixels "instanced float texture, derivatives and record 0" 129 114 [80, 159, 179, 255]
          expect pixels "instanced float texture, derivatives and record 1" 293 109 [80, 96, 78, 255]
          expect pixels "discard preserves the clear" 116 114 [17, 29, 46, 255]
  putStrLn "verified resident instances, vertex float texture, fragment quad derivatives and discard"

instancedVertex :: VertexShader
instancedVertex = VertexShader
  (Transform (Homogeneous (letExpr InstanceRoot $ \root -> FloatBinary Float3 AddF VertexPosition
    (MakeVec3 (Component X4 root) (Component Y4 root) (Component Z4 root)))))
  (MakeVec2 (Component X4 (SampleAt Texture2 (MakeVec2 0.5 0.5))) (Component X4 InstanceParameters))
  InstanceColor

instancedFragment :: FragmentShader
instancedFragment = MaskedFragmentShader (Compare Less (Component X4 FragmentCoordinates) 122) $
  letExpr (MakeVec2 (Component X4 FragmentCoordinates / 8) (Component Y4 FragmentCoordinates / 4)) $ \point ->
    MakeVec4 (Component X2 InterpolatedUV + Derivative ScreenX Float1 (Component X2 point))
      (Component Y2 InterpolatedUV + Derivative ScreenY Float1 (Component Y2 point))
      (Component Z4 InterpolatedColor) 1

emitShaders :: FilePath -> IO ()
emitShaders directory = do
  parameter <- maybe (fail "shader parameter slot unavailable") pure (uniformSlot 2)
  bound <- maybe (fail "shader loop bound unavailable") pure (loopLimit 4)
  let write name = Lazy.writeFile (directory ++ "/" ++ name) .
        Builder.toLazyByteString . foldMap Builder.word32LE
  write "default.vert.spv" (compileVertexShader defaultVertexShader)
  write "default.frag.spv" (compileFragmentShader defaultFragmentShader)
  write "material.frag.spv" (compileFragmentShader (diagnosticShader parameter bound))
  write "bindings.frag.spv" (compileFragmentShader (bindingProbe bound))
  write "varyings.vert.spv" (compileVertexShader (vertexProbe parameter))
  write "varyings.frag.spv" (compileFragmentShader varyingProbe)
  write "instanced.vert.spv" (compileInstancedVertexShader instancedVertex)
  write "instanced.frag.spv" (compileFragmentShader instancedFragment)

checkOffscreen :: Window -> Renderer -> IO ()
checkOffscreen window renderer =
  bracket (createRenderTarget renderer RGBA16Float 64 48) (destroyRenderTarget renderer) $ \hdr ->
    bracket (createRenderTarget renderer RGBA8 64 48) (destroyRenderTarget renderer) $ \copy -> do
      let red = Frame (Color (199/255) (53/255) (87/255) 1) [] []
      let green = Frame (Color (37/255) (193/255) (117/255) 1) [] []
      let readFrom target x y w h = OverlayBatch (Just (renderTargetTexture target)) (quad x y w h white)
      let preserve = Frame background [] [readFrom hdr 0 0 64 48]
      let screen = Frame background [] [readFrom copy 16 22 64 48, readFrom hdr 110 22 64 48]
      rejected <- try (renderFrame renderer screen)
      case rejected of
        Left (InvalidPass (SampleBeforeWrite _)) -> pure ()
        _ -> fail ("Reading an unwritten target was accepted: " ++ show rejected)
      feedback <- try (renderPasses renderer [RenderPass hdr preserve] screen)
      case feedback of
        Left (InvalidPass (SampleOwnAttachment _)) -> pure ()
        _ -> fail ("Sampling the current attachment was accepted: " ++ show feedback)
      let passes = [RenderPass hdr red, RenderPass copy preserve, RenderPass hdr green]
      pixels <- readPresented window renderer passes screen
      expect pixels "previous HDR version through RGBA8 pass" 30 33 [199, 53, 87, 255]
      expect pixels "rewritten HDR target version" 125 33 [37, 193, 117, 255]
      persisted <- readPresented window renderer [] screen
      expect persisted "retained target version on later frame" 30 33 [199, 53, 87, 255]
      putStrLn "verified offscreen HDR/RGBA8 pass versions, sampling and retained output"

checkFrame :: Window -> Renderer -> (Int -> Int -> IO Frame) -> IO FramePixels
checkFrame window renderer makeFrame = do
  (width, height) <- windowFramebufferSize window
  frame <- makeFrame width height
  pixels <- readPresented window renderer [] frame
  unless ((pixelWidth pixels, pixelHeight pixels) == (width, height)) $
    fail "Surface and framebuffer extent differ"
  expect pixels "background" (width - 5) (height - 5) [17, 29, 46, 255]
  expect pixels "depth near drawn before far" (width `div` 2 + 7) (height `div` 2 - 5) [38, 173, 79, 255]
  expect pixels "texture top left" 20 24 [211, 39, 66, 255]
  expect pixels "texture top right" 68 24 [31, 185, 71, 255]
  expect pixels "texture bottom left" 20 58 [54, 104, 222, 255]
  expect pixels "texture bottom right" 68 58 [198, 147, 42, 255]
  expect pixels "alpha over clear" (width - 38) 20 [117, 34, 29, 255]
  expect pixels "scissor excludes adjacent pixel" (width - 50) 20 [17, 29, 46, 255]
  pure pixels

readPresented :: Window -> Renderer -> [RenderPass] -> Frame -> IO FramePixels
readPresented window renderer passes frame = loop (120 :: Int)
  where
    loop 0 = fail "No presentable image returned"
    loop attempts = do
      _ <- pollWindowEvents window
      result <- renderPassesReadback renderer passes frame
      case result of
        (FramePresented, Just pixels) -> pure pixels
        _ -> threadDelay 2000 >> loop (attempts - 1)

expect :: FramePixels -> String -> Int -> Int -> [Int] -> IO ()
expect frame label x y wanted = do
  let offset = (y * pixelWidth frame + x) * 4
  let actual = map fromIntegral $ BS.unpack $ BS.take 4 (BS.drop offset (pixelRGBA8 frame))
  unless (length actual == 4 && and (zipWith (\a b -> abs (a - b) <= 2) actual wanted)) $
    fail (label ++ " at " ++ show (x, y) ++ " expected " ++ show wanted ++ " observed " ++ show actual)

triangle :: Float -> Color -> Either PlanError MeshData
triangle scale color = meshData
  [Vertex (x * scale) (y * scale) (-2 * scale) 0 0 color
  | (x, y) <- [(-0.65, -0.45), (0.8, -0.42), (0.15, 0.72)]]
  [0, 1, 2]

checker :: BS.ByteString
checker = BS.pack
  [ 211, 39, 66, 255, 31, 185, 71, 255
  , 54, 104, 222, 255, 198, 147, 42, 255
  ]

background, white :: Color
background = Color (17/255) (29/255) (46/255) 1
white = Color 1 1 1 1

quad :: Float -> Float -> Float -> Float -> Color -> [Vertex]
quad x y width height color =
  [corner x y 0 0, corner right y 1 0, corner right bottom 1 1
  , corner x y 0 0, corner right bottom 1 1, corner x bottom 0 1]
  where
    right = x + width
    bottom = y + height
    corner px py u v = Vertex px py 0 u v color

tile :: Int -> [Vertex]
tile index = quad (fromIntegral (index `mod` 40) * 10) (fromIntegral (index `div` 40) * 9)
  10 9 (Color 1 1 1 0.42)

benchmark :: Int -> Window -> Renderer -> Frame -> IO ()
benchmark frames window renderer frame = do
  metrics <- go frames []
  putStrLn ("rendered " ++ show frames ++ " frames with 1000 adjacent textured quads")
  describe "host geometry preparation including packing" (map (milliseconds . framePrepareNSec) metrics)
  describe "prior fence wait" (map (milliseconds . frameFenceWaitNSec) metrics)
  describe "command encoding CPU" (map (milliseconds . frameEncodeNSec) metrics)
  describe "submit CPU" (map (milliseconds . frameSubmitNSec) metrics)
  describe "present call wall time" (map (milliseconds . framePresentCallNSec) metrics)
  describe "GPU timestamp interval" [value / 1000000 | metric <- metrics, Just value <- [frameGPUNSec metric]]
  where
    go 0 samples = pure samples
    go remaining samples = do
      events <- pollWindowEvents window
      when (CloseRequested `elem` events) (fail "Benchmark window closed")
      status <- renderFrame renderer frame
      sample <- rendererLastMetrics renderer
      if status == FramePresented
        then go (remaining - 1) (maybe samples (: samples) sample)
        else threadDelay 2000 >> go remaining samples
    milliseconds :: Word64 -> Double
    milliseconds value = fromIntegral value / 1000000
    describe name samples = case sort samples of
      [] -> putStrLn (name ++ " unavailable")
      sorted -> do
        let count = length sorted
        let pick fraction = case drop (min (count - 1) (floor (fraction * fromIntegral count))) sorted of
              value : _ -> value
              [] -> 0
        putStrLn (name ++ " ms p50=" ++ show (pick (0.5 :: Double)) ++ " p95=" ++ show (pick (0.95 :: Double)))

writePPM :: FilePath -> FramePixels -> IO ()
writePPM path frame = BS.writeFile path (header <> BS.pack (dropAlpha (BS.unpack (pixelRGBA8 frame))))
  where
    header = C8.pack ("P6\n" ++ show (pixelWidth frame) ++ " " ++ show (pixelHeight frame) ++ "\n255\n")
    dropAlpha :: [Word8] -> [Word8]
    dropAlpha (r : g : b : _ : rest) = r : g : b : dropAlpha rest
    dropAlpha _ = []
