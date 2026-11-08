{-# LANGUAGE PatternSynonyms #-}

module GameIO.Graphics
  ( Renderer
  , Mesh
  , InstanceTemplate
  , Instances
  , Texture
  , RenderTarget
  , Material
  , MaterialConfig(..)
  , PipelineMode(..)
  , ShaderInputs(..)
  , TargetFormat(..)
  , FloatTextureFormat(..)
  , RenderPass(..)
  , TextureFilter(..)
  , WorldDraw(..)
  , OverlayBatch(..)
  , ClipRect(..)
  , Frame(Frame, FrameWithBackground, frameClear, frameWorld, frameOverlay)
  , FrameStatus(..)
  , FramePixels(..)
  , FrameMetrics(..)
  , GraphicsError(..)
  , VulkanError(..)
  , withRenderer
  , rendererAdapterName
  , createMesh
  , destroyMesh
  , createInstanceTemplate
  , destroyInstanceTemplate
  , createInstances
  , destroyInstances
  , createTextureRGBA8
  , createTextureRGBA8With
  , createTextureFloat32
  , destroyTexture
  , createRenderTarget
  , destroyRenderTarget
  , renderTargetTexture
  , renderTargetDepth
  , createMaterial
  , destroyMaterial
  , renderFrame
  , renderFrameReadback
  , renderPasses
  , renderPassesReadback
  , renderOffscreen
  , rendererLastMetrics
  , rendererLastSubmission
  , waitRenderer
  , module GameIO.Graphics.Plan
  ) where

import Control.Exception (Exception, bracket, catch, finally, mask_, onException, throwIO)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import Data.Unique (Unique, newUnique)
import Foreign
import GameIO.Graphics.Plan
import GameIO.Graphics.Shader
  ( FragmentShader, TextureSlot, VertexShader, compileFragmentShader, compileVertexShader, compileInstancedVertexShader
  , textureSlotIndex, uniformBytes, uniformSlots
  )
import GameIO.Graphics.Vulkan.Layout
import GameIO.Graphics.Vulkan.Lifetime
import GameIO.Graphics.Vulkan.Pipeline
import GameIO.Graphics.Vulkan.Raw
import GameIO.Graphics.Vulkan.Resources
import GameIO.Graphics.Vulkan.Session
import GameIO.Platform.Win32 (Window, assertWindowThread, windowFramebufferSize, withWindowRenderer)
import GHC.Clock (getMonotonicTimeNSec)

data WorldDraw
  = WorldDraw !Mesh !(Maybe Texture) !Mat4
  | WorldDrawWith !Mesh !Material !ShaderInputs !Mat4
  | WorldMeshes !(NonEmpty Mesh) !(Maybe Texture) !Mat4
  | WorldMeshesWith !(NonEmpty Mesh) !Material !ShaderInputs !Mat4
  | WorldInstances !InstanceTemplate !Instances !Material !ShaderInputs !Mat4

data ClipRect = ClipRect !Int !Int !Int !Int deriving (Eq, Show)

data OverlayBatch
  = OverlayBatch !(Maybe Texture) ![Vertex]
  | ClippedOverlay !ClipRect !(Maybe Texture) ![Vertex]
  | OverlayBatchWith !(Maybe ClipRect) !Material !ShaderInputs ![Vertex]

data Frame
  = Frame
      { frameClear :: !Color
      , frameWorld :: ![WorldDraw]
      , frameOverlay :: ![OverlayBatch]
      }
  | FrameWithBackground
      { frameClear :: !Color
      , frameBackground :: ![OverlayBatch]
      , frameWorld :: ![WorldDraw]
      , frameOverlay :: ![OverlayBatch]
      }

data RenderPass = RenderPass !RenderTarget !Frame

data TargetFormat = RGBA8 | RGBA16Float deriving (Eq, Ord, Show)

data FloatTextureFormat = R32Float | RGBA32Float deriving (Eq, Show)

data MaterialConfig = MaterialConfig !PipelineMode !VertexShader !FragmentShader
data ShaderInputs = ShaderInputs ![(TextureSlot, Texture)] ![Vec4]

data FrameStatus = FramePresented | FrameMinimized | FrameDeferred | FrameRecreatePending
  deriving (Eq, Show)

data FramePixels = FramePixels
  { pixelWidth :: !Int
  , pixelHeight :: !Int
  , pixelRGBA8 :: !BS.ByteString
  } deriving (Eq, Show)

data FrameMetrics = FrameMetrics
  { framePrepareNSec :: !Word64
  , frameFenceWaitNSec :: !Word64
  , frameEncodeNSec :: !Word64
  , frameSubmitNSec :: !Word64
  , framePresentCallNSec :: !Word64
  , frameGPUNSec :: !(Maybe Double)
  , frameSubmissionIndex :: !Word64
  } deriving (Eq, Show)

data GraphicsError
  = RendererClosed
  | RendererFaulted
  | ForeignResource
  | ResourceReleased
  | InvalidTexture !Int !Int !Int
  | InvalidOverlayVertexCount !Int
  | OverlayTooLarge
  | SurfaceReadbackUnavailable
  | InvalidTargetSize !Int !Int
  | TargetFormatUnavailable !TargetFormat
  | FloatTextureFormatUnavailable !FloatTextureFormat
  | BorrowedTargetTexture
  | InvalidPass !PassError
  | MaterialRoleMismatch !PipelineMode
  | DuplicateTextureSlot
  | InvalidShaderParameters
  | InvalidInstanceTemplate
  | InvalidInstances
  | FrameParameterBufferTooLarge
  deriving (Eq, Show)

instance Exception GraphicsError

data Resource value = Resource !Unique !Int !(IORef (Maybe value))
newtype Mesh = Mesh (Resource ResidentMesh)
newtype InstanceTemplate = InstanceTemplate (Resource ResidentVertexStream)
newtype Instances = Instances (Resource ResidentVertexStream)
newtype RenderTarget = RenderTarget (Resource ResidentTarget)
newtype Material = Material (Resource ResidentMaterial)

data Texture
  = UploadedTexture !(Resource ResidentTexture)
  | ColorTargetTexture !RenderTarget
  | DepthTargetTexture !RenderTarget

data ResidentTarget = ResidentTarget
  { offscreenColor :: !ResidentTexture
  , offscreenDepth :: !ResidentTexture
  , offscreenWidth :: !Word32
  , offscreenHeight :: !Word32
  , offscreenFormat :: !Word32
  , offscreenPipelines :: !Pipelines
  , offscreenVersion :: !(IORef (Maybe Word64))
  }

data ResidentMaterial = ResidentMaterial
  { residentMode :: !PipelineMode
  , residentVertexWords :: ![Word32]
  , residentInstancedWords :: [Word32]
  , residentFragmentWords :: ![Word32]
  , residentMaterialPipelines :: !(IORef (IntMap.IntMap VkPipeline))
  , residentMaterialScope :: !Scope
  }

data ParameterBuffer = ParameterBuffer !MappedBuffer !VkDescriptorSet

data Health = Healthy | Faulted | Closed deriving (Eq)

data Generation = Generation
  { generationRequestedSize :: !(Int, Int)
  , generationConfig :: !SurfaceConfig
  , generationSwapchain :: !Swapchain
  , generationDepth :: !ImageWithView
  , generationPipelines :: !Pipelines
  , generationScope :: !Scope
  , generationReadback :: !(IORef (Maybe MappedBuffer))
  }

data Renderer = Renderer
  { rendererWindow :: !Window
  , rendererOwner :: !Unique
  , rendererHealth :: !(IORef Health)
  , rendererSession :: !Session
  , rendererBindings :: !Bindings
  , rendererScope :: !Scope
  , rendererPipelines :: !(IORef (IntMap.IntMap Pipelines))
  , rendererWhite :: !ResidentTexture
  , rendererCommandBuffers :: !CommandBuffers
  , rendererAvailable :: !VkSemaphore
  , rendererAcquireFence :: !VkFence
  , rendererAcquirePending :: !(IORef Bool)
  , rendererDrawFence :: !VkFence
  , rendererDrawPending :: !(IORef Bool)
  , rendererQuery :: !(Maybe VkQueryPool)
  , rendererGeneration :: !(IORef (Maybe Generation))
  , rendererResize :: !(IORef Bool)
  , rendererOverlayBuffer :: !(IORef (Maybe MappedBuffer))
  , rendererParameterBuffer :: !(IORef (Maybe ParameterBuffer))
  , rendererAssets :: !(IORef (IntMap.IntMap (IO ())))
  , rendererNextAsset :: !(IORef Int)
  , rendererInFlightMetrics :: !(IORef (Maybe FrameMetrics))
  , rendererCompletedMetrics :: !(IORef (Maybe FrameMetrics))
  , rendererSubmissionIndex :: !(IORef Word64)
  }

withRenderer :: Window -> (Renderer -> IO a) -> IO a
withRenderer window use = withWindowRenderer window $
  withSession window $ \session ->
    withScope (cleanupAllowed (sessionCleanup session)) $ \scope ->
      bracket (openRenderer scope window session) closeRenderer use

openRenderer :: Scope -> Window -> Session -> IO Renderer
openRenderer scope window session = do
  bindings <- createBindings scope session
  commands <- createCommandBuffers scope session
  available <- own scope (createSemaphore session) (\value -> vkDestroySemaphore fns device value nullPtr)
  acquireFence <- own scope (createFence session False) (\value -> vkDestroyFence fns device value nullPtr)
  drawFence <- own scope (createFence session False) (\value -> vkDestroyFence fns device value nullPtr)
  white <- own scope (uploadTexture session bindings (uploadCommands commands) Nearest
    VK_FORMAT_R8G8B8A8_UNORM 1 1 (BS.replicate 4 255))
    (destroyResidentTexture session bindings)
  query <- if adapterTimestampBits (sessionAdapter session) == 0
    then pure Nothing
    else Just <$> own scope createQuery (\pool -> vkDestroyQueryPool fns device pool nullPtr)
  Renderer window <$> newUnique <*> newIORef Healthy <*> pure session <*> pure bindings
    <*> pure scope <*> newIORef IntMap.empty <*> pure white
    <*> pure commands <*> pure available <*> pure acquireFence <*> newIORef False <*> pure drawFence <*> newIORef False
    <*> pure query <*> newIORef Nothing <*> newIORef False <*> newIORef Nothing <*> newIORef Nothing
    <*> newIORef IntMap.empty
    <*> newIORef 0 <*> newIORef Nothing <*> newIORef Nothing <*> newIORef 0
  where
    fns = sessionFns session
    device = sessionDevice session
    createQuery = withInfo VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
      [U32 0, U32 VK_QUERY_TYPE_TIMESTAMP, U32 2, U32 0] $ \infoPtr ->
        newHandle "vkCreateQueryPool" (vkCreateQueryPool fns device infoPtr nullPtr)

closeRenderer :: Renderer -> IO ()
closeRenderer renderer = releaseWhenAllowed (sessionCleanup session) $ do
  assertWindowThread (rendererWindow renderer)
  writeIORef (rendererHealth renderer) Closed
  let finish = discardLostDevice (awaitGpu renderer >> pure ()) >> retireGeneration renderer
  let destroyAssets = do
        assets <- atomicModifyIORef' (rendererAssets renderer) (\current -> (IntMap.empty, current))
        foldr finally (pure ()) (IntMap.elems assets)
  let destroyOverlay = readIORef (rendererOverlayBuffer renderer) >>= mapM_ (destroyMappedBuffer session)
  let destroyUniforms = readIORef (rendererParameterBuffer renderer) >>= mapM_ (destroyParameterBuffer renderer)
  finish `finally` releaseWhenAllowed (sessionCleanup session)
    (destroyAssets `finally` (destroyUniforms `finally` destroyOverlay))
  where
    session = rendererSession renderer

retireGeneration :: Renderer -> IO ()
retireGeneration renderer = mask_ $ do
  current <- readIORef (rendererGeneration renderer)
  forM_ current $ \generation -> do
    discardLostDevice (waitSwapchain (rendererSession renderer) (generationSwapchain generation))
    writeIORef (rendererGeneration renderer) Nothing
    closeScope (generationScope generation)

discardLostDevice :: IO () -> IO ()
discardLostDevice action = action `catch` \failure -> case failure of
  VulkanCallFailed _ VK_ERROR_DEVICE_LOST -> pure ()
  _ -> throwIO failure

rendererAdapterName :: Renderer -> String
rendererAdapterName = adapterName . sessionAdapter . rendererSession

requireRenderer :: Renderer -> IO ()
requireRenderer renderer = do
  requireCleanupKnown (sessionCleanup (rendererSession renderer))
  assertWindowThread (rendererWindow renderer)
  health <- readIORef (rendererHealth renderer)
  case health of
    Healthy -> pure ()
    Faulted -> throwIO RendererFaulted
    Closed -> throwIO RendererClosed

createMesh :: Renderer -> MeshData -> IO Mesh
createMesh renderer mesh = do
  requireRenderer renderer
  let session = rendererSession renderer
  Mesh <$> track renderer
    (uploadMesh session (uploadCommands (rendererCommandBuffers renderer)) mesh)
    (destroyResidentMesh session)

destroyMesh :: Renderer -> Mesh -> IO ()
destroyMesh renderer (Mesh resource) = release renderer resource

createInstanceTemplate :: Renderer -> [Vec3] -> IO InstanceTemplate
createInstanceTemplate renderer corners = do
  requireRenderer renderer
  let count = length corners
  let finiteCorner (Vec3 x y z) = all finiteFloat [x, y, z]
  unless (validStreamCount count && count `mod` 3 == 0 && all finiteCorner corners) $
    throwIO InvalidInstanceTemplate
  InstanceTemplate <$> createVertexStream renderer count cornerStride (`pokeCorners` corners)

destroyInstanceTemplate :: Renderer -> InstanceTemplate -> IO ()
destroyInstanceTemplate renderer (InstanceTemplate resource) = release renderer resource

createInstances :: Renderer -> [InstanceData] -> IO Instances
createInstances renderer records = do
  requireRenderer renderer
  let count = length records
  let finite4 (Vec4 x y z w) = all finiteFloat [x, y, z, w]
  let finiteRecord (InstanceData root parameters _ _) = finite4 root && finite4 parameters
  unless (validStreamCount count && all finiteRecord records) (throwIO InvalidInstances)
  Instances <$> createVertexStream renderer count instanceStride (`pokeInstances` records)

destroyInstances :: Renderer -> Instances -> IO ()
destroyInstances renderer (Instances resource) = release renderer resource

validStreamCount :: Int -> Bool
validStreamCount count = count > 0 && toInteger count <= toInteger (maxBound :: Word32)

finiteFloat :: Float -> Bool
finiteFloat value = not (isNaN value || isInfinite value)

createVertexStream :: Renderer -> Int -> Int -> (Ptr () -> IO ()) -> IO (Resource ResidentVertexStream)
createVertexStream renderer count stride write =
  track renderer (uploadVertexStream session commands (fromIntegral count) stride write) (destroyVertexStream session)
  where
    session = rendererSession renderer
    commands = uploadCommands (rendererCommandBuffers renderer)

createTextureRGBA8 :: Renderer -> Int -> Int -> BS.ByteString -> IO Texture
createTextureRGBA8 renderer = createTextureRGBA8With renderer Nearest

createTextureRGBA8With :: Renderer -> TextureFilter -> Int -> Int -> BS.ByteString -> IO Texture
createTextureRGBA8With renderer filterMode =
  createUploadedTexture renderer filterMode VK_FORMAT_R8G8B8A8_UNORM 4

-- Tightly packed little-endian IEEE binary32 pixels, in RGBA channel order
-- for RGBA32Float. The same sampler rules apply as for an RGBA8 texture.
createTextureFloat32 :: Renderer -> TextureFilter -> FloatTextureFormat -> Int -> Int -> BS.ByteString -> IO Texture
createTextureFloat32 renderer filterMode textureFormat width height bytes = do
  requireRenderer renderer
  let session = rendererSession renderer
  let (format, pixelSize) = case textureFormat of
        R32Float -> (VK_FORMAT_R32_SFLOAT, 4)
        RGBA32Float -> (VK_FORMAT_R32G32B32A32_SFLOAT, 16)
  let filterFeature = case filterMode of Nearest -> 0; Linear -> VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  supported <- supportsFormat (sessionInstanceFns session) (adapterHandle (sessionAdapter session)) format
    (VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT .|. VK_FORMAT_FEATURE_TRANSFER_DST_BIT .|. filterFeature)
  unless supported (throwIO (FloatTextureFormatUnavailable textureFormat))
  createUploadedTexture renderer filterMode format pixelSize width height bytes

createUploadedTexture :: Renderer -> TextureFilter -> Word32 -> Integer -> Int -> Int -> BS.ByteString -> IO Texture
createUploadedTexture renderer filterMode format pixelSize width height bytes = do
  requireRenderer renderer
  let session = rendererSession renderer
  let bindings = rendererBindings renderer
  let commands = uploadCommands (rendererCommandBuffers renderer)
  let maximumDimension = fromIntegral (adapterMaximumImageDimension (sessionAdapter session))
  unless (width > 0 && height > 0 && width <= maximumDimension && height <= maximumDimension &&
      toInteger width * toInteger height * pixelSize == toInteger (BS.length bytes)) $
    throwIO (InvalidTexture width height (BS.length bytes))
  UploadedTexture <$> track renderer
    (uploadTexture session bindings commands filterMode format (fromIntegral width) (fromIntegral height) bytes)
    (destroyResidentTexture session bindings)

uploadTexture :: Session -> Bindings -> VkCommandBuffer -> TextureFilter -> Word32 -> Word32 -> Word32
              -> BS.ByteString -> IO ResidentTexture
uploadTexture session bindings commands filterMode format width height bytes = mask_ $ do
  image <- uploadTextureImage session commands format width height bytes
  describeTexture session bindings filterMode image `onException` destroyImageWithView session image

destroyTexture :: Renderer -> Texture -> IO ()
destroyTexture renderer texture = case texture of
  UploadedTexture resource -> release renderer resource
  ColorTargetTexture _ -> throwIO BorrowedTargetTexture
  DepthTargetTexture _ -> throwIO BorrowedTargetTexture

createRenderTarget :: Renderer -> TargetFormat -> Int -> Int -> IO RenderTarget
createRenderTarget renderer targetFormat width height = do
  requireRenderer renderer
  let session = rendererSession renderer
  let bindings = rendererBindings renderer
  let physical = adapterHandle (sessionAdapter session)
  let fns = sessionInstanceFns session
  let largest = fromIntegral (adapterMaximumImageDimension (sessionAdapter session))
  unless (width > 0 && height > 0 && width <= largest && height <= largest) $
    throwIO (InvalidTargetSize width height)
  let format = case targetFormat of
        RGBA8 -> VK_FORMAT_R8G8B8A8_UNORM
        RGBA16Float -> VK_FORMAT_R16G16B16A16_SFLOAT
  let colorFeatures = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT .|. VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
        .|. VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT .|. VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  let depthFeatures = VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT .|. VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
  colorSupported <- supportsFormat fns physical format colorFeatures
  depthSupported <- supportsFormat fns physical VK_FORMAT_D32_SFLOAT depthFeatures
  unless (colorSupported && depthSupported) (throwIO (TargetFormatUnavailable targetFormat))
  let w = fromIntegral width
  let h = fromIntegral height
  let acquire = mask_ $ do
        pipelines <- pipelinesFor renderer format
        let attachment pixelFormat usage aspect filterMode = do
              image <- createImageWithView session w h pixelFormat (usage .|. VK_IMAGE_USAGE_SAMPLED_BIT) aspect
              describeTexture session bindings filterMode image `onException` destroyImageWithView session image
        let destroyAttachment = destroyResidentTexture session bindings
        color <- attachment format VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT VK_IMAGE_ASPECT_COLOR_BIT Linear
        depth <- attachment VK_FORMAT_D32_SFLOAT VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT VK_IMAGE_ASPECT_DEPTH_BIT Nearest
          `onException` destroyAttachment color
        ResidentTarget color depth w h format pipelines <$> newIORef Nothing
      destroy target = destroyResidentTexture session bindings (offscreenDepth target)
        `finally` destroyResidentTexture session bindings (offscreenColor target)
  RenderTarget <$> track renderer acquire destroy

destroyRenderTarget :: Renderer -> RenderTarget -> IO ()
destroyRenderTarget renderer (RenderTarget resource) = release renderer resource

renderTargetTexture :: RenderTarget -> Texture
renderTargetTexture = ColorTargetTexture

renderTargetDepth :: RenderTarget -> Texture
renderTargetDepth = DepthTargetTexture

createMaterial :: Renderer -> MaterialConfig -> IO Material
createMaterial renderer (MaterialConfig mode vertex fragment) = do
  requireRenderer renderer
  let acquire = ResidentMaterial mode (compileVertexShader vertex) (compileInstancedVertexShader vertex) (compileFragmentShader fragment)
        <$> newIORef IntMap.empty <*> newScope (cleanupAllowed (sessionCleanup (rendererSession renderer)))
  Material <$> track renderer acquire (closeScope . residentMaterialScope)

destroyMaterial :: Renderer -> Material -> IO ()
destroyMaterial renderer (Material resource) = release renderer resource

materialPipelineFor :: Renderer -> Word32 -> VertexInput -> ResidentMaterial -> IO VkPipeline
materialPipelineFor renderer format vertexInput material = mask_ $ do
  let cacheRef = residentMaterialPipelines material
  cache <- readIORef cacheRef
  case IntMap.lookup key cache of
    Just pipeline -> pure pipeline
    Nothing -> do
      pipeline <- createMaterialPipeline (residentMaterialScope material) (rendererSession renderer)
        (rendererBindings renderer) format (residentMode material) vertexInput materialVertexWords (residentFragmentWords material)
      modifyIORef' cacheRef (IntMap.insert key pipeline)
      pure pipeline
  where
    key = fromIntegral format * 2 + fromEnum vertexInput
    materialVertexWords = case vertexInput of
      MeshVertices -> residentVertexWords material
      InstancedCorners -> residentInstancedWords material

track :: Renderer -> IO a -> (a -> IO ()) -> IO (Resource a)
track renderer create destroy = mask_ $ do
  key <- atomicModifyIORef' (rendererNextAsset renderer) (\index -> (index + 1, index))
  asset <- create
  alive <- newIORef (Just asset)
  let retire = writeIORef alive Nothing >> destroy asset
  modifyIORef' (rendererAssets renderer) (IntMap.insert key retire)
  pure (Resource (rendererOwner renderer) key alive)

release :: Renderer -> Resource a -> IO ()
release renderer (Resource owner key _) = mask_ $ do
  requireRenderer renderer
  unless (owner == rendererOwner renderer) (throwIO ForeignResource)
  awaitDraw renderer
  action <- atomicModifyIORef' (rendererAssets renderer) $ \assets ->
    (IntMap.delete key assets, IntMap.lookup key assets)
  sequence_ action

resolve :: Renderer -> Resource a -> IO a
resolve renderer (Resource owner _ alive) = do
  unless (owner == rendererOwner renderer) (throwIO ForeignResource)
  readIORef alive >>= maybe (throwIO ResourceReleased) pure

resolveTexture :: Renderer -> Maybe Texture -> IO ResidentTexture
resolveTexture renderer texture = case texture of
  Nothing -> pure (rendererWhite renderer)
  Just (UploadedTexture resource) -> resolve renderer resource
  Just (ColorTargetTexture (RenderTarget resource)) -> offscreenColor <$> resolve renderer resource
  Just (DepthTargetTexture (RenderTarget resource)) -> offscreenDepth <$> resolve renderer resource

renderFrame :: Renderer -> Frame -> IO FrameStatus
renderFrame renderer = renderPasses renderer []

renderFrameReadback :: Renderer -> Frame -> IO (FrameStatus, Maybe FramePixels)
renderFrameReadback renderer = renderPassesReadback renderer []

renderPasses :: Renderer -> [RenderPass] -> Frame -> IO FrameStatus
renderPasses renderer passes frame = fst <$> render renderer passes frame False

renderPassesReadback :: Renderer -> [RenderPass] -> Frame -> IO (FrameStatus, Maybe FramePixels)
renderPassesReadback renderer passes frame = render renderer passes frame True

-- Complete offscreen passes without acquiring or presenting a window image.
-- Their resulting textures can be sampled by a later visible frame.
renderOffscreen :: Renderer -> [RenderPass] -> IO ()
renderOffscreen renderer passes = mask_ $ do
  requireRenderer renderer
  unless (null passes) $ do
    awaitDraw renderer
    let emptyFrame = Frame (Color 0 0 0 0) [] []
    (prepared, _) <- preparePasses renderer VK_FORMAT_R8G8B8A8_UNORM passes emptyFrame
    let session = rendererSession renderer
    let commands = drawCommands (rendererCommandBuffers renderer)
    runUpload session commands (mapM_ (drawOffscreen renderer) prepared)
      `onException` writeIORef (rendererHealth renderer) Faulted
    commitTargets prepared

-- | Metrics for the most recently collected graphics submission.
rendererLastMetrics :: Renderer -> IO (Maybe FrameMetrics)
rendererLastMetrics renderer = readIORef (rendererCompletedMetrics renderer)

-- | Sequence number assigned after a successful window frame submission.
rendererLastSubmission :: Renderer -> IO Word64
rendererLastSubmission = readIORef . rendererSubmissionIndex

-- | Wait for pending graphics work and collect its timestamps. Swapchain
-- presentation uses its own completion tracking within the renderer scope.
waitRenderer :: Renderer -> IO ()
waitRenderer renderer = mask_ (requireRenderer renderer >> awaitDraw renderer)

render :: Renderer -> [RenderPass] -> Frame -> Bool -> IO (FrameStatus, Maybe FramePixels)
render renderer passes frame wantsPixels = mask_ $ do
  requireRenderer renderer
  waitStart <- getMonotonicTimeNSec
  awaitDraw renderer
  waitEnd <- getMonotonicTimeNSec
  size <- windowFramebufferSize (rendererWindow renderer)
  generation <- selectGeneration renderer size
  case generation of
    Nothing -> pure (FrameMinimized, Nothing)
    Just ready -> do
      prepareStart <- getMonotonicTimeNSec
      (targetPasses, geometry) <- preparePasses renderer (surfaceFormat (generationConfig ready)) passes frame
      staging <- if wantsPixels then Just <$> readbackBuffer renderer ready else pure Nothing
      prepareEnd <- getMonotonicTimeNSec
      let metrics = FrameMetrics (prepareEnd - prepareStart) (waitEnd - waitStart) 0 0 0 Nothing 0
      (renderReady renderer ready targetPasses frame geometry staging metrics)
        `onException` writeIORef (rendererHealth renderer) Faulted

selectGeneration :: Renderer -> (Int, Int) -> IO (Maybe Generation)
selectGeneration renderer size@(width, height)
  | width <= 0 || height <= 0 = pure Nothing
  | otherwise = do
      current <- readIORef (rendererGeneration renderer)
      dirty <- readIORef (rendererResize renderer)
      case current of
        Just generation | generationRequestedSize generation == size && not dirty -> pure current
        _ -> do
          let session = rendererSession renderer
          config <- surfaceConfig session size
          case config of
            Nothing -> pure Nothing
            Just surface -> do
              retireGeneration renderer
              scope <- newScope (cleanupAllowed (sessionCleanup session))
              generation <- openGeneration renderer scope size surface `onException` closeScope scope
              writeIORef (rendererGeneration renderer) (Just generation)
              writeIORef (rendererResize renderer) False
              pure (Just generation)

openGeneration :: Renderer -> Scope -> (Int, Int) -> SurfaceConfig -> IO Generation
openGeneration renderer scope size config = do
  let session = rendererSession renderer
  swapchain <- createSwapchain scope session config
  depth <- own scope (createImageWithView session (surfaceWidth config) (surfaceHeight config) VK_FORMAT_D32_SFLOAT
    VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT VK_IMAGE_ASPECT_DEPTH_BIT) (destroyImageWithView session)
  pipelines <- pipelinesFor renderer (surfaceFormat config)
  Generation size config swapchain depth pipelines scope <$> newIORef Nothing

pipelinesFor :: Renderer -> Word32 -> IO Pipelines
pipelinesFor renderer format = mask_ $ do
  cache <- readIORef (rendererPipelines renderer)
  case IntMap.lookup key cache of
    Just pipelines -> pure pipelines
    Nothing -> do
      pipelines <- createPipelines (rendererScope renderer) (rendererSession renderer) (rendererBindings renderer) format
      modifyIORef' (rendererPipelines renderer) (IntMap.insert key pipelines)
      pure pipelines
  where
    key = fromIntegral format

awaitDraw :: Renderer -> IO ()
awaitDraw renderer = awaitGpu renderer >>= \pending -> when pending (collectMetrics renderer)

awaitGpu :: Renderer -> IO Bool
awaitGpu renderer = do
  let session = rendererSession renderer
  acquired <- readIORef (rendererAcquirePending renderer)
  when acquired $ do
    waitFence session WindowSystemOperation (rendererAcquireFence renderer)
    writeIORef (rendererAcquirePending renderer) False
  pending <- readIORef (rendererDrawPending renderer)
  when pending $ do
    waitFence session QueueSubmission (rendererDrawFence renderer)
    writeIORef (rendererDrawPending renderer) False
  pure pending

data DrawStyle
  = SimpleTexture !ResidentTexture
  | MaterialDraw !VkPipeline ![ResidentTexture] !Word32

data PreparedWorld
  = PreparedWorld !(NonEmpty ResidentMesh) !DrawStyle !Mat4
  | PreparedInstances !ResidentVertexStream !ResidentVertexStream !DrawStyle !Mat4
data PreparedOverlay = PreparedOverlay !DrawStyle !(Maybe ClipRect) !Word32 !Word32

data Prepared = Prepared
  ![PreparedWorld] !(Maybe MappedBuffer) ![PreparedOverlay] ![PreparedOverlay] !(Maybe ParameterBuffer)

data PreparedTargetPass = PreparedTargetPass !ResidentTarget !TargetPass !Color !Prepared

preparePasses :: Renderer -> Word32 -> [RenderPass] -> Frame -> IO ([PreparedTargetPass], Prepared)
preparePasses renderer finalFormat passes final = do
  targets <- traverse (resolveTarget . passTarget) passes
  readSets <- traverse (traverse resolveTarget . sampledTargets) (map passFrame passes)
  finalReads <- traverse resolveTarget (sampledTargets final)
  let resident = IntMap.fromList (targets ++ concat readSets ++ finalReads)
  existing <- traverse (readIORef . offscreenVersion) resident
  let inputs = zipWith (\(key, _) sampled -> PassInput key (map fst sampled)) targets readSets
  (planned, _) <- either (throwIO . InvalidPass) pure $
    planPasses (IntMap.mapMaybe id existing) inputs (map fst finalReads)
  uniformWrites <- newIORef (0, [])
  let frames = zip (map (offscreenFormat . snd) targets) (map passFrame passes) ++ [(finalFormat, final)]
  (total, geometry) <- prepareGeometry renderer uniformWrites 0 frames
  vertexBuffer <- if total == 0 then pure Nothing else Just <$>
    overlayBuffer renderer (fromIntegral total * fromIntegral vertexStride)
  (uniformSize, uniformData) <- readIORef uniformWrites
  parameters <- if uniformSize == 0
    then pure Nothing
    else Just <$> packParameters renderer uniformSize uniformData
  allGeometry <- traverse (pack vertexBuffer parameters) geometry
  case splitAt (length passes) allGeometry of
    (offscreen, [presented]) -> pure
      (zipWith3 toPass (zip targets planned) passes offscreen, presented)
    _ -> throwIO (VulkanInvariantViolation "prepared passes do not match frame")
  where
    passTarget (RenderPass target _) = target
    passFrame (RenderPass _ frame) = frame
    toPass ((_, target), plan) (RenderPass _ frame) = PreparedTargetPass target plan (frameClear frame)
    resolveTarget (RenderTarget resource@(Resource _ key _)) = (,) key <$> resolve renderer resource
    pack buffer parameters (world, backgroundGroups, foregroundGroups) = do
      forM_ buffer $ \mapped ->
        forM_ (backgroundGroups ++ foregroundGroups) $ \(PreparedOverlay _ _ first _, vertices) ->
          pokeVertices (mappedAddress mapped `plusPtr` (fromIntegral first * vertexStride)) vertices
      pure (Prepared world buffer (combineAdjacent (map fst backgroundGroups))
        (combineAdjacent (map fst foregroundGroups)) parameters)

sampledTargets :: Frame -> [RenderTarget]
sampledTargets frame =
  foldr collect [] (concatMap worldTextures (frameWorld frame) ++
    concatMap overlayTextures (backgroundBatches frame ++ frameOverlay frame))
  where
    worldTextures (WorldDraw _ texture _) = maybe [] pure texture
    worldTextures (WorldDrawWith _ _ (ShaderInputs textures _) _) = map snd textures
    worldTextures (WorldMeshes _ texture _) = maybe [] pure texture
    worldTextures (WorldMeshesWith _ _ (ShaderInputs textures _) _) = map snd textures
    worldTextures (WorldInstances _ _ _ (ShaderInputs textures _) _) = map snd textures
    overlayTextures (OverlayBatch texture _) = maybe [] pure texture
    overlayTextures (ClippedOverlay _ texture _) = maybe [] pure texture
    overlayTextures (OverlayBatchWith _ _ (ShaderInputs textures _) _) = map snd textures
    collect (ColorTargetTexture target) rest = target : rest
    collect (DepthTargetTexture target) rest = target : rest
    collect _ rest = rest

backgroundBatches :: Frame -> [OverlayBatch]
backgroundBatches Frame{} = []
backgroundBatches FrameWithBackground{frameBackground = background} = background

type UniformWrites = IORef (Word64, [(Word32, [Vec4])])
type OverlayGeometry = [(PreparedOverlay, [Vertex])]

prepareGeometry :: Renderer -> UniformWrites -> Word32 -> [(Word32, Frame)]
                -> IO (Word32, [([PreparedWorld], OverlayGeometry, OverlayGeometry)])
prepareGeometry _ _ first [] = pure (first, [])
prepareGeometry renderer uniforms first ((format, frame) : rest) = do
  let nativeMesh (Mesh resource) = resolve renderer resource
  let simple meshes texture matrix =
        PreparedWorld <$> traverse nativeMesh meshes <*> (SimpleTexture <$> resolveTexture renderer texture) <*> pure matrix
  let material meshes program inputs matrix =
        PreparedWorld <$> traverse nativeMesh meshes
          <*> prepareStyle renderer format uniforms (WorldRole MeshVertices) program inputs <*> pure matrix
  world <- forM (frameWorld frame) $ \draw -> case draw of
    WorldDraw mesh texture matrix -> simple (mesh :| []) texture matrix
    WorldDrawWith mesh program inputs matrix -> material (mesh :| []) program inputs matrix
    WorldMeshes meshes texture matrix -> simple meshes texture matrix
    WorldMeshesWith meshes program inputs matrix -> material meshes program inputs matrix
    WorldInstances (InstanceTemplate corners) (Instances records) program inputs matrix ->
      PreparedInstances <$> resolve renderer corners <*> resolve renderer records
        <*> prepareStyle renderer format uniforms (WorldRole InstancedCorners) program inputs <*> pure matrix
  (afterBackground, underlay) <- prepareOverlay renderer format uniforms first (backgroundBatches frame)
  (afterFrame, groups) <- prepareOverlay renderer format uniforms afterBackground (frameOverlay frame)
  (total, following) <- prepareGeometry renderer uniforms afterFrame rest
  pure (total, (world, underlay, groups) : following)

prepareOverlay :: Renderer -> Word32 -> UniformWrites -> Word32 -> [OverlayBatch] -> IO (Word32, [(PreparedOverlay, [Vertex])])
prepareOverlay renderer format uniforms first batches = case batches of
  [] -> pure (first, [])
  batch : rest -> do
    let simple texture = SimpleTexture <$> resolveTexture renderer texture
    let (style, clip, vertices) = case batch of
          OverlayBatch texture values -> (simple texture, Nothing, values)
          ClippedOverlay rectangle texture values -> (simple texture, Just rectangle, values)
          OverlayBatchWith rectangle material inputs values ->
            (prepareStyle renderer format uniforms OverlayRole material inputs, rectangle, values)
    let count = length vertices
    when (count `mod` 3 /= 0) (throwIO (InvalidOverlayVertexCount count))
    when (toInteger count + toInteger first > toInteger (maxBound :: Word32)) (throwIO OverlayTooLarge)
    if count == 0 then prepareOverlay renderer format uniforms first rest else do
      resident <- style
      let amount = fromIntegral count
      (total, groups) <- prepareOverlay renderer format uniforms (first + amount) rest
      pure (total, (PreparedOverlay resident clip first amount, vertices) : groups)

combineAdjacent :: [PreparedOverlay] -> [PreparedOverlay]
combineAdjacent draws = case draws of
  [] -> []
  first : rest -> combine first rest
  where
    combine current [] = [current]
    combine current@(PreparedOverlay (SimpleTexture texture) clip first count)
        (next@(PreparedOverlay (SimpleTexture nextTexture) nextClip _ nextCount) : rest)
      | residentDescriptor texture == residentDescriptor nextTexture && clip == nextClip =
          combine (PreparedOverlay (SimpleTexture texture) clip first (count + nextCount)) rest
      | otherwise = current : combine next rest
    combine current (next : rest) = current : combine next rest

data DrawRole = WorldRole !VertexInput | OverlayRole

prepareStyle :: Renderer -> Word32 -> UniformWrites -> DrawRole -> Material -> ShaderInputs -> IO DrawStyle
prepareStyle renderer format writes role (Material resource) (ShaderInputs textures parameters) = do
  material <- resolve renderer resource
  let mode = residentMode material
  let isWorld = mode `elem` [WorldDepth, WorldAlpha, WorldAdditive]
  let (world, vertexInput) = case role of
        WorldRole input -> (True, input)
        OverlayRole -> (False, MeshVertices)
  unless (isWorld == world) (throwIO (MaterialRoleMismatch mode))
  let bySlot = IntMap.fromList [(fromIntegral (textureSlotIndex slot), texture) | (slot, texture) <- textures]
  unless (IntMap.size bySlot == length textures) (throwIO DuplicateTextureSlot)
  let finite4 (Vec4 x y z w) = all finiteFloat [x, y, z, w]
  unless (length parameters <= fromIntegral uniformSlots && all finite4 parameters) (throwIO InvalidShaderParameters)
  textures' <- traverse (resolveTexture renderer . (`IntMap.lookup` bySlot)) [0..3]
  let boundary = max 1 (adapterUniformAlignment (sessionAdapter (rendererSession renderer)))
  let size = ((fromIntegral uniformBytes + boundary - 1) `div` boundary) * boundary
  (offset, pending) <- readIORef writes
  when (offset + size > fromIntegral (maxBound :: Word32)) (throwIO FrameParameterBufferTooLarge)
  let dynamicOffset = fromIntegral offset
  writeIORef writes (offset + size, (dynamicOffset, parameters) : pending)
  pipeline <- materialPipelineFor renderer format vertexInput material
  pure (MaterialDraw pipeline textures' dynamicOffset)

packParameters :: Renderer -> Word64 -> [(Word32, [Vec4])] -> IO ParameterBuffer
packParameters renderer required writes = do
  resource@(ParameterBuffer mapped _) <- parameterBuffer renderer required
  let base = mappedAddress mapped
  fillBytes base 0 (fromIntegral required)
  forM_ writes $ \(start, values) ->
    forM_ (zip [0..] values) $ \(slot, Vec4 x y z w) -> do
      let offset = fromIntegral start + slot * 16
      pokeByteOff base offset x
      pokeByteOff base (offset + 4) y
      pokeByteOff base (offset + 8) z
      pokeByteOff base (offset + 12) w
  pure resource

parameterBuffer :: Renderer -> Word64 -> IO ParameterBuffer
parameterBuffer renderer required = mask_ $ do
  previous <- readIORef (rendererParameterBuffer renderer)
  case previous of
    Just resource@(ParameterBuffer mapped _) | bufferCapacity (mappedBuffer mapped) >= required -> pure resource
    _ -> do
      let capacity = maybe (max required 65536) (max required . (* 2) . capacityOf) previous
      mapped <- createMappedBuffer session capacity VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
      descriptor <- describeParameters session (rendererBindings renderer) (mappedBuffer mapped)
        `onException` destroyMappedBuffer session mapped
      let replacement = ParameterBuffer mapped descriptor
      writeIORef (rendererParameterBuffer renderer) (Just replacement)
      mapM_ (destroyParameterBuffer renderer) previous
      pure replacement
  where
    session = rendererSession renderer
    capacityOf (ParameterBuffer mapped _) = bufferCapacity (mappedBuffer mapped)

destroyParameterBuffer :: Renderer -> ParameterBuffer -> IO ()
destroyParameterBuffer renderer (ParameterBuffer mapped descriptor) =
  releaseWhenAllowed (sessionCleanup session) $
    freeDescriptor session (rendererBindings renderer) descriptor `finally` destroyMappedBuffer session mapped
  where
    session = rendererSession renderer

overlayBuffer :: Renderer -> Word64 -> IO MappedBuffer
overlayBuffer renderer required = mask_ $ do
  old <- readIORef (rendererOverlayBuffer renderer)
  case old of
    Just buffer | bufferCapacity (mappedBuffer buffer) >= required -> pure buffer
    _ -> do
      let session = rendererSession renderer
      let initial = 65536
      let capacity = maybe (max required initial) (max required . (* 2) . bufferCapacity . mappedBuffer) old
      replacement <- createMappedBuffer session capacity VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
      writeIORef (rendererOverlayBuffer renderer) (Just replacement)
      mapM_ (destroyMappedBuffer session) old
      pure replacement

readbackBuffer :: Renderer -> Generation -> IO MappedBuffer
readbackBuffer renderer generation = do
  let config = generationConfig generation
  unless (surfaceUsage config .&. VK_IMAGE_USAGE_TRANSFER_SRC_BIT /= 0) (throwIO SurfaceReadbackUnavailable)
  existing <- readIORef (generationReadback generation)
  case existing of
    Just buffer -> pure buffer
    Nothing -> do
      let session = rendererSession renderer
      let bytes = fromIntegral (surfaceWidth config) * fromIntegral (surfaceHeight config) * 4
      buffer <- own (generationScope generation) (createMappedBuffer session bytes VK_BUFFER_USAGE_TRANSFER_DST_BIT)
        (destroyMappedBuffer session)
      writeIORef (generationReadback generation) (Just buffer)
      pure buffer

renderReady :: Renderer -> Generation -> [PreparedTargetPass] -> Frame -> Prepared -> Maybe MappedBuffer
            -> FrameMetrics -> IO (FrameStatus, Maybe FramePixels)
renderReady renderer generation passes frame geometry readback metrics = alloca $ \imageIndex -> do
  resetFence session (rendererAcquireFence renderer)
  acquired <- vkAcquireNextImageKHR fns (sessionDevice session) (swapchainHandle swapchain) acquireBudgetNSec
    (rendererAvailable renderer) (rendererAcquireFence renderer) imageIndex
  case acquired of
    VK_TIMEOUT -> pure (FrameDeferred, Nothing)
    VK_NOT_READY -> pure (FrameDeferred, Nothing)
    VK_ERROR_OUT_OF_DATE_KHR -> markResize >> pure (FrameRecreatePending, Nothing)
    _ -> do
      when (acquired /= VK_SUBOPTIMAL_KHR) (vkCheck "vkAcquireNextImageKHR" (pure acquired))
      writeIORef (rendererAcquirePending renderer) True
      index <- peek imageIndex
      image <- lookupImage index (swapchainImages swapchain)
      waitPresentation session image
      encodeStart <- getMonotonicTimeNSec
      recordCommands session commands (encodeFrame renderer generation image passes frame geometry readback)
      submitStart <- getMonotonicTimeNSec
      resetFence session (rendererDrawFence renderer)
      submitDraw session commands (rendererAvailable renderer) (presentSemaphore image) (rendererDrawFence renderer)
      writeIORef (rendererDrawPending renderer) True
      submission <- atomicModifyIORef' (rendererSubmissionIndex renderer) (\n -> (n+1, n+1))
      commitTargets passes
      presentStart <- getMonotonicTimeNSec
      shown <- present session swapchain image index
      presentEnd <- getMonotonicTimeNSec
      writeIORef (rendererInFlightMetrics renderer) $ Just metrics
        { frameEncodeNSec = submitStart - encodeStart
        , frameSubmitNSec = presentStart - submitStart
        , framePresentCallNSec = presentEnd - presentStart
        , frameSubmissionIndex = submission
        }
      when (acquired == VK_SUBOPTIMAL_KHR || shown == VK_SUBOPTIMAL_KHR || shown == VK_ERROR_OUT_OF_DATE_KHR) markResize
      status <- case shown of
        VK_SUBOPTIMAL_KHR -> pure FramePresented
        VK_ERROR_OUT_OF_DATE_KHR -> pure FrameRecreatePending
        _ -> vkCheck "vkQueuePresentKHR" (pure shown) >> pure FramePresented
      pixels <- forM readback $ \buffer -> do
        awaitDraw renderer
        copyPixels (generationConfig generation) buffer
      pure (status, pixels)
  where
    session = rendererSession renderer
    fns = sessionFns session
    swapchain = generationSwapchain generation
    commands = drawCommands (rendererCommandBuffers renderer)
    markResize = writeIORef (rendererResize renderer) True
    lookupImage index images = case drop (fromIntegral index) images of
      image : _ -> pure image
      [] -> throwIO (VulkanInvariantViolation "acquired image index is outside its swapchain")

acquireBudgetNSec :: Word64
acquireBudgetNSec = 8000000

commitTargets :: [PreparedTargetPass] -> IO ()
commitTargets = mapM_ $ \(PreparedTargetPass target plan _ _) ->
  case targetOutput plan of ImageVersion _ version -> writeIORef (offscreenVersion target) (Just version)

encodeFrame :: Renderer -> Generation -> PresentImage -> [PreparedTargetPass] -> Frame -> Prepared -> Maybe MappedBuffer -> IO ()
encodeFrame renderer generation image passes frame geometry readback = do
  forM_ (rendererQuery renderer) $ \query -> do
    vkCmdResetQueryPool fns commands query 0 2
    vkCmdWriteTimestamp fns commands VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT query 0
  forM_ passes (drawOffscreen renderer)
  forM_ (planFrame (maybe False (const True) readback)) encode
  forM_ (rendererQuery renderer) $ \query ->
    vkCmdWriteTimestamp fns commands VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT query 1
  where
    session = rendererSession renderer
    fns = sessionFns session
    commands = drawCommands (rendererCommandBuffers renderer)
    config = generationConfig generation
    depth = generationDepth generation
    encode step = case step of
      TransitionColor use -> transitionImage session commands (presentImage image) VK_IMAGE_ASPECT_COLOR_BIT use
      TransitionDepth use -> transitionImage session commands (imageHandle (ownedImage depth)) VK_IMAGE_ASPECT_DEPTH_BIT use
      DrawGeometry -> drawGeometry renderer (surfaceWidth config) (surfaceHeight config)
        (presentView image) (ownedImageView depth) (generationPipelines generation)
        VK_ATTACHMENT_STORE_OP_DONT_CARE (frameClear frame) geometry
      CopyColorToHost -> forM_ readback $ \target ->
        copyImageToBuffer session commands (presentImage image)
          (surfaceWidth config) (surfaceHeight config) (mappedBuffer target)

drawOffscreen :: Renderer -> PreparedTargetPass -> IO ()
drawOffscreen renderer (PreparedTargetPass target plan clear geometry) = do
  let session = rendererSession renderer
  let commands = drawCommands (rendererCommandBuffers renderer)
  let color = residentTextureImage (offscreenColor target)
  let depth = residentTextureImage (offscreenDepth target)
  let use image aspect before after =
        transitionImage session commands (imageHandle (ownedImage image)) aspect (ImageTransition before after)
  let previous = maybe Discarded (const ShaderSampled) (targetPrevious plan)
  use color VK_IMAGE_ASPECT_COLOR_BIT previous ColorTarget
  use depth VK_IMAGE_ASPECT_DEPTH_BIT previous DepthTarget
  drawGeometry renderer (offscreenWidth target) (offscreenHeight target)
    (ownedImageView color) (ownedImageView depth) (offscreenPipelines target)
    VK_ATTACHMENT_STORE_OP_STORE clear geometry
  use color VK_IMAGE_ASPECT_COLOR_BIT ColorTarget ShaderSampled
  use depth VK_IMAGE_ASPECT_DEPTH_BIT DepthTarget ShaderSampled

drawGeometry :: Renderer -> Word32 -> Word32 -> VkImageView -> VkImageView -> Pipelines -> Word32
             -> Color -> Prepared -> IO ()
drawGeometry renderer width height colorView depthView pipelines depthStore clear (Prepared world overlay underlay draws parameters) =
  withInfo VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO
    (attachment colorView VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL VK_ATTACHMENT_STORE_OP_STORE (colorFloats clear))
    $ \colorInfo -> withInfo VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO
      (attachment depthView VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL depthStore [1, 0, 0, 0])
      $ \depthInfo -> withInfo VK_STRUCTURE_TYPE_RENDERING_INFO
        [U32 0, Aggregate fullRect, U32 1, U32 0, U32 1, pointer colorInfo, pointer depthInfo, pointer nullPtr]
        $ \renderInfo -> do
          vkCmdBeginRendering fns commands renderInfo
          withStruct (structure (map F32 [0, 0, fromIntegral width, fromIntegral height, 0, 1])) $ \viewport ->
            vkCmdSetViewport fns commands 0 1 viewport
          drawBatches underlay
          setScissor fullRect
          mapM_ drawWorld world
          drawBatches draws
          vkCmdEndRendering fns commands
  where
    session = rendererSession renderer
    fns = sessionFns session
    bindings = rendererBindings renderer
    commands = drawCommands (rendererCommandBuffers renderer)
    fullRect = rect2D 0 0 width height
    setScissor rect = withStruct rect (vkCmdSetScissor fns commands 0 1)
    bindPipeline = vkCmdBindPipeline fns commands VK_PIPELINE_BIND_POINT_GRAPHICS
    drawWorld prepared = case prepared of
      PreparedWorld meshes style matrix -> do
        bindStyle (worldPipeline pipelines) style
        setTransform session bindings commands matrix
        forM_ meshes $ \mesh -> do
          bindVertices session commands (meshVertexBuffer mesh)
          vkCmdBindIndexBuffer fns commands (bufferHandle (meshIndexBuffer mesh)) 0 VK_INDEX_TYPE_UINT32
          vkCmdDrawIndexed fns commands (residentIndexCount mesh) 1 0 0 0
      PreparedInstances (ResidentVertexStream corners cornerCount) (ResidentVertexStream records instanceCount) style matrix -> do
        bindStyle (worldPipeline pipelines) style
        setTransform session bindings commands matrix
        withArray [bufferHandle corners, bufferHandle records] $ \buffers ->
          withArray [0, 0] (vkCmdBindVertexBuffers fns commands 0 2 buffers)
        vkCmdDraw fns commands cornerCount instanceCount 0 0
    bindStyle simple style = case style of
      SimpleTexture texture -> do
        bindPipeline simple
        bindTexture session bindings commands texture
      MaterialDraw pipeline textures offset -> case parameters of
        Nothing -> throwIO (VulkanInvariantViolation "material draw has no parameter buffer")
        Just (ParameterBuffer _ descriptors) -> do
          bindPipeline pipeline
          bindMaterialInputs session bindings commands textures descriptors offset
    drawBatches groups = unless (null groups) $ forM_ overlay $ \buffer -> do
      pixelTransform <- either (throwIO . VulkanInvariantViolation . show) pure
        (pixelMat4 (fromIntegral width) (fromIntegral height))
      setTransform session bindings commands pixelTransform
      bindVertices session commands (mappedBuffer buffer)
      forM_ groups $ \(PreparedOverlay style clip first count) ->
        forM_ (scissor width height clip) $ \rect -> do
          setScissor rect
          bindStyle (overlayPipeline pipelines) style
          vkCmdDraw fns commands count 1 first 0
    attachment view layout store clearValue =
      [pointer view, U32 layout, U32 VK_RESOLVE_MODE_NONE, pointer nullPtr
      , U32 VK_IMAGE_LAYOUT_UNDEFINED, U32 VK_ATTACHMENT_LOAD_OP_CLEAR, U32 store, floats clearValue]
    colorFloats (Color r g b a) = [r, g, b, a]

scissor :: Word32 -> Word32 -> Maybe ClipRect -> Maybe Structure
scissor width height clip = case clip of
  Nothing -> Just (rect2D 0 0 width height)
  Just (ClipRect x y w h)
    | endX > startX && endY > startY ->
        Just (rect2D (fromInteger startX) (fromInteger startY) (fromInteger (endX - startX)) (fromInteger (endY - startY)))
    | otherwise -> Nothing
    where
      startX = max 0 (toInteger x)
      startY = max 0 (toInteger y)
      endX = min (toInteger width) (toInteger x + toInteger w)
      endY = min (toInteger height) (toInteger y + toInteger h)

collectMetrics :: Renderer -> IO ()
collectMetrics renderer = do
  inFlight <- atomicModifyIORef' (rendererInFlightMetrics renderer) (\metrics -> (Nothing, metrics))
  forM_ inFlight $ \metrics -> do
    gpu <- forM (rendererQuery renderer) $ \query -> allocaArray 2 $ \result -> do
      vkCheck "vkGetQueryPoolResults" $
        vkGetQueryPoolResults fns (sessionDevice session) query 0 2 16 (castPtr result) 8 VK_QUERY_RESULT_64_BIT
      first <- peekElemOff result 0
      lastTick <- peekElemOff result 1
      let validBits = fromIntegral (adapterTimestampBits (sessionAdapter session))
      let validMask = if validBits >= 64 then maxBound else bit validBits - 1 :: Word64
      let ticks = (lastTick - first) .&. validMask
      pure (fromIntegral ticks * realToFrac (adapterTimestampPeriodNSec (sessionAdapter session)))
    writeIORef (rendererCompletedMetrics renderer) (Just metrics { frameGPUNSec = gpu })
  where
    session = rendererSession renderer
    fns = sessionFns session

copyPixels :: SurfaceConfig -> MappedBuffer -> IO FramePixels
copyPixels config buffer = do
  rgba <- if surfaceFormat config == VK_FORMAT_B8G8R8A8_UNORM
    then BSI.create bytes $ \out -> do
      -- Transfer from the mapped buffer in bulk. Channel conversion reads
      -- the owned CPU bytes, which may have different cache properties.
      copyBytes out (castPtr source) bytes
      let wordsOut = castPtr out :: Ptr Word32
          swap !index
            | index == width * height = pure ()
            | otherwise = do
                bgra <- peekElemOff wordsOut index
                -- Windows x64 is little endian. Keep green and alpha.
                let pixel = (bgra .&. 0xff00ff00) .|.
                      shiftL (bgra .&. 0x000000ff) 16 .|.
                      shiftR (bgra .&. 0x00ff0000) 16
                pokeElemOff wordsOut index pixel
                swap (index + 1)
      swap 0
    else BS.packCStringLen (castPtr source, bytes)
  pure (FramePixels width height rgba)
  where
    source = mappedAddress buffer
    width = fromIntegral (surfaceWidth config)
    height = fromIntegral (surfaceHeight config)
    bytes = width * height * 4
