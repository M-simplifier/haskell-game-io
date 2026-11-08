{-# LANGUAGE PatternSynonyms #-}

module GameIO.Graphics.Vulkan.Resources where

import Control.Exception (bracket, finally, mask_, onException, throwIO)
import qualified Data.ByteString as BS
import Foreign
import GameIO.Graphics.Plan
import GameIO.Graphics.Vulkan.Layout
import GameIO.Graphics.Vulkan.Lifetime
import GameIO.Graphics.Vulkan.Raw
import GameIO.Graphics.Vulkan.Session

data Buffer = Buffer
  { bufferHandle :: !VkBuffer
  , bufferMemory :: !VkDeviceMemory
  , bufferCapacity :: !Word64
  }

data MappedBuffer = MappedBuffer
  { mappedBuffer :: !Buffer
  , mappedAddress :: !(Ptr ())
  }

data Image = Image
  { imageHandle :: !VkImage
  , imageMemory :: !VkDeviceMemory
  }

data ImageWithView = ImageWithView
  { ownedImage :: !Image
  , ownedImageView :: !VkImageView
  }

createBuffer :: Session -> Word64 -> Word32 -> Word32 -> IO Buffer
createBuffer session bytes usage properties = mask_ $ do
  handle <- withInfo VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
    [U32 0, U64 bytes, U32 usage, U32 VK_SHARING_MODE_EXCLUSIVE, U32 0, pointer nullPtr] $ \create ->
      newHandle "vkCreateBuffer" (vkCreateBuffer fns device create nullPtr)
  memory <- allocateMemory session (vkGetBufferMemoryRequirements fns device handle) properties
    `onException` vkDestroyBuffer fns device handle nullPtr
  let buffer = Buffer handle memory bytes
  vkCheck "vkBindBufferMemory" (vkBindBufferMemory fns device handle memory 0)
    `onException` destroyBuffer session buffer
  pure buffer
  where
    fns = sessionFns session
    device = sessionDevice session

destroyBuffer :: Session -> Buffer -> IO ()
destroyBuffer session buffer = releaseWhenAllowed (sessionCleanup session) $
  vkDestroyBuffer fns device (bufferHandle buffer) nullPtr
    `finally` vkFreeMemory fns device (bufferMemory buffer) nullPtr
  where
    fns = sessionFns session
    device = sessionDevice session

createMappedBuffer :: Session -> Word64 -> Word32 -> IO MappedBuffer
createMappedBuffer session bytes usage = mask_ $ do
  buffer <- createBuffer session bytes usage (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  address <- alloca (\out -> do
    vkCheck "vkMapMemory" (vkMapMemory fns device (bufferMemory buffer) 0 bytes 0 out)
    peek out) `onException` destroyBuffer session buffer
  pure (MappedBuffer buffer address)
  where
    fns = sessionFns session
    device = sessionDevice session

destroyMappedBuffer :: Session -> MappedBuffer -> IO ()
destroyMappedBuffer session mapped = releaseWhenAllowed (sessionCleanup session) $
  vkUnmapMemory (sessionFns session) (sessionDevice session) (bufferMemory (mappedBuffer mapped))
    `finally` destroyBuffer session (mappedBuffer mapped)

allocateMemory :: Session -> (Ptr () -> IO ()) -> Word32 -> IO VkDeviceMemory
allocateMemory session requirements properties = withOutput memoryRequirementsAbi $ \out -> do
  requirements out
  bytes <- peekByteOff out memoryRequirements_size
  types <- peekByteOff out memoryRequirements_memoryTypeBits
  index <- memoryType session types properties
  withInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO [U64 bytes, U32 index] $ \create ->
    newHandle "vkAllocateMemory" (vkAllocateMemory (sessionFns session) (sessionDevice session) create nullPtr)

createImage :: Session -> Word32 -> Word32 -> Word32 -> Word32 -> IO Image
createImage session width height format usage = mask_ $ do
  handle <- withInfo VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
    [ U32 0, U32 VK_IMAGE_TYPE_2D, U32 format, words32 [width, height, 1]
    , U32 1, U32 1, U32 VK_SAMPLE_COUNT_1_BIT, U32 VK_IMAGE_TILING_OPTIMAL
    , U32 usage, U32 VK_SHARING_MODE_EXCLUSIVE, U32 0, pointer nullPtr
    , U32 VK_IMAGE_LAYOUT_UNDEFINED
    ] $ \create -> newHandle "vkCreateImage" (vkCreateImage fns device create nullPtr)
  memory <- allocateMemory session (vkGetImageMemoryRequirements fns device handle) VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    `onException` vkDestroyImage fns device handle nullPtr
  let image = Image handle memory
  vkCheck "vkBindImageMemory" (vkBindImageMemory fns device handle memory 0)
    `onException` destroyImage session image
  pure image
  where
    fns = sessionFns session
    device = sessionDevice session

destroyImage :: Session -> Image -> IO ()
destroyImage session image = releaseWhenAllowed (sessionCleanup session) $
  vkDestroyImage fns device (imageHandle image) nullPtr
    `finally` vkFreeMemory fns device (imageMemory image) nullPtr
  where
    fns = sessionFns session
    device = sessionDevice session

createImageWithView :: Session -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ImageWithView
createImageWithView session width height format usage aspect = mask_ $ do
  image <- createImage session width height format usage
  view <- createImageView session (imageHandle image) format aspect
    `onException` destroyImage session image
  pure (ImageWithView image view)

destroyImageWithView :: Session -> ImageWithView -> IO ()
destroyImageWithView session image = releaseWhenAllowed (sessionCleanup session) $
  vkDestroyImageView (sessionFns session) (sessionDevice session) (ownedImageView image) nullPtr
    `finally` destroyImage session (ownedImage image)

data CommandBuffers = CommandBuffers
  { drawCommands :: !VkCommandBuffer
  , uploadCommands :: !VkCommandBuffer
  }

createCommandBuffers :: Scope -> Session -> IO CommandBuffers
createCommandBuffers scope session = do
  pool <- own scope create (\value -> vkDestroyCommandPool fns device value nullPtr)
  withInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
    [pointer pool, U32 VK_COMMAND_BUFFER_LEVEL_PRIMARY, U32 2] $ \allocate ->
      allocaArray 2 $ \out -> do
        vkCheck "vkAllocateCommandBuffers" (vkAllocateCommandBuffers fns device allocate out)
        CommandBuffers <$> peekElemOff out 0 <*> peekElemOff out 1
  where
    fns = sessionFns session
    device = sessionDevice session
    create = withInfo VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
      [U32 VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, U32 (adapterQueueFamily (sessionAdapter session))] $ \poolInfo ->
        newHandle "vkCreateCommandPool" (vkCreateCommandPool fns device poolInfo nullPtr)

recordCommands :: Session -> VkCommandBuffer -> IO () -> IO ()
recordCommands session commands encode = do
  vkCheck "vkResetCommandBuffer" (vkResetCommandBuffer fns commands 0)
  withInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
    [U32 VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, pointer nullPtr] $ \begin ->
      vkCheck "vkBeginCommandBuffer" (vkBeginCommandBuffer fns commands begin)
  encode
  vkCheck "vkEndCommandBuffer" (vkEndCommandBuffer fns commands)
  where
    fns = sessionFns session

submitDraw :: Session -> VkCommandBuffer -> VkSemaphore -> VkSemaphore -> VkFence -> IO ()
submitDraw session commands available rendered fence =
  with available $ \waits ->
    -- The image's initial layout transition also waits for acquisition.
    with VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT $ \stages ->
      with rendered $ \signals ->
        submit session commands fence 1 (castPtr waits) stages 1 (castPtr signals)

submit :: Session -> VkCommandBuffer -> VkFence -> Word32 -> Ptr () -> Ptr Word32 -> Word32 -> Ptr () -> IO ()
submit session commands fence waitCount waits stages signalCount signals =
  with commands $ \buffers ->
    withInfo VK_STRUCTURE_TYPE_SUBMIT_INFO
      [ U32 waitCount, pointer waits, pointer stages
      , U32 1, pointer buffers, U32 signalCount, pointer signals
      ] $ \submitInfo ->
        vkCheck "vkQueueSubmit" (vkQueueSubmit (sessionFns session) (sessionQueue session) 1 submitInfo fence)

runUpload :: Session -> VkCommandBuffer -> IO () -> IO ()
runUpload session commands encode = mask_ $
  bracket (createFence session False) destroy $ \fence -> do
    recordCommands session commands encode
    submit session commands fence 0 nullPtr nullPtr 0 nullPtr
    waitFence session QueueSubmission fence
  where
    destroy fence = releaseWhenAllowed (sessionCleanup session) $
      vkDestroyFence (sessionFns session) (sessionDevice session) fence nullPtr

transitionImage :: Session -> VkCommandBuffer -> VkImage -> Word32 -> ImageTransition -> IO ()
transitionImage session commands image aspect (ImageTransition before after) =
  withInfo VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
    [ U32 sourceAccess, U32 destinationAccess, U32 sourceLayout, U32 destinationLayout
    , U32 queueFamilyIgnored, U32 queueFamilyIgnored, pointer image, imageRange aspect
    ] $ \barrier ->
      vkCmdPipelineBarrier (sessionFns session) commands sourceStage destinationStage 0
        0 nullPtr 0 nullPtr 1 barrier
  where
    (sourceLayout, sourceStage, sourceAccess) = imageUse before
    (destinationLayout, destinationStage, destinationAccess) = imageUse after

imageUse :: ImageUse -> (Word32, Word32, Word32)
imageUse usage = case usage of
  Discarded -> (VK_IMAGE_LAYOUT_UNDEFINED, VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, 0)
  ColorTarget ->
    (VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
  DepthTarget ->
    ( VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    , VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    , VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT .|. VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    )
  CopySource -> (VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, VK_PIPELINE_STAGE_TRANSFER_BIT, VK_ACCESS_TRANSFER_READ_BIT)
  CopyDestination -> (VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_PIPELINE_STAGE_TRANSFER_BIT, VK_ACCESS_TRANSFER_WRITE_BIT)
  ShaderSampled ->
    ( VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    , VK_PIPELINE_STAGE_VERTEX_SHADER_BIT .|. VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , VK_ACCESS_SHADER_READ_BIT
    )
  Presentation -> (VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, 0)

bufferVisibility :: Session -> VkCommandBuffer -> Buffer -> Word32 -> Word32 -> IO ()
bufferVisibility session commands buffer stage access =
  withInfo VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
    [ U32 VK_ACCESS_TRANSFER_WRITE_BIT, U32 access
    , U32 queueFamilyIgnored, U32 queueFamilyIgnored
    , pointer (bufferHandle buffer), U64 0, U64 (bufferCapacity buffer)
    ] $ \barrier ->
      vkCmdPipelineBarrier (sessionFns session) commands VK_PIPELINE_STAGE_TRANSFER_BIT stage 0
        0 nullPtr 1 barrier 0 nullPtr

queueFamilyIgnored :: Word32
queueFamilyIgnored = maxBound

imageCopyRegion :: Word32 -> Word32 -> Structure
imageCopyRegion width height = structure
  [ U64 0, U32 0, U32 0
  , words32 [VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1]
  , words32 [0, 0, 0]
  , words32 [width, height, 1]
  ]

copyImageToBuffer :: Session -> VkCommandBuffer -> VkImage -> Word32 -> Word32 -> Buffer -> IO ()
copyImageToBuffer session commands image width height destination = do
  withStruct (imageCopyRegion width height) $ \region ->
    vkCmdCopyImageToBuffer (sessionFns session) commands image VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      (bufferHandle destination) 1 region
  bufferVisibility session commands destination VK_PIPELINE_STAGE_HOST_BIT VK_ACCESS_HOST_READ_BIT

uploadTextureImage :: Session -> VkCommandBuffer -> Word32 -> Word32 -> Word32 -> BS.ByteString -> IO ImageWithView
uploadTextureImage session commands format width height bytes = mask_ $ do
  image <- createImageWithView session width height format
    (VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT) VK_IMAGE_ASPECT_COLOR_BIT
  let handle = imageHandle (ownedImage image)
  let (before, after) = planTextureUpload
  (withStaging session (fromIntegral (BS.length bytes)) $ \staging -> do
    BS.useAsCStringLen bytes $ \(source, count) ->
      copyBytes (mappedAddress staging) (castPtr source) count
    runUpload session commands $ do
      transitionImage session commands handle VK_IMAGE_ASPECT_COLOR_BIT before
      withStruct (imageCopyRegion width height) $ \region ->
        vkCmdCopyBufferToImage (sessionFns session) commands (bufferHandle (mappedBuffer staging)) handle
          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 region
      transitionImage session commands handle VK_IMAGE_ASPECT_COLOR_BIT after)
    `onException` destroyImageWithView session image
  pure image

data ResidentMesh = ResidentMesh
  { meshVertexBuffer :: !Buffer
  , meshIndexBuffer :: !Buffer
  , residentIndexCount :: !Word32
  }

uploadMesh :: Session -> VkCommandBuffer -> MeshData -> IO ResidentMesh
uploadMesh session commands mesh = mask_ $ do
  vertex <- createBuffer session vertexBytes (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
    VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  indices <- createBuffer session indexBytes (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
    VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT `onException` destroyBuffer session vertex
  -- Retire records keep the resident handle until release. Force its count
  -- here so that record cannot keep the source vertices and indices alive.
  let !resident = ResidentMesh vertex indices (meshIndexCount mesh)
  (withStaging session (vertexBytes + indexBytes) $ \staging -> do
    pokeVertices (mappedAddress staging) (meshVertices mesh)
    pokeArray (mappedAddress staging `plusPtr` fromIntegral vertexBytes) (meshIndices mesh)
    runUpload session commands $ do
      let source = mappedBuffer staging
      copyBuffer session commands source vertex 0 vertexBytes
      copyBuffer session commands source indices vertexBytes indexBytes
      bufferVisibility session commands vertex VK_PIPELINE_STAGE_VERTEX_INPUT_BIT VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
      bufferVisibility session commands indices VK_PIPELINE_STAGE_VERTEX_INPUT_BIT VK_ACCESS_INDEX_READ_BIT)
    `onException` destroyResidentMesh session resident
  pure resident
  where
    vertexBytes = fromIntegral (length (meshVertices mesh)) * fromIntegral vertexStride
    indexBytes = fromIntegral (meshIndexCount mesh) * 4

destroyResidentMesh :: Session -> ResidentMesh -> IO ()
destroyResidentMesh session mesh =
  destroyBuffer session (meshIndexBuffer mesh) `finally` destroyBuffer session (meshVertexBuffer mesh)

data ResidentVertexStream = ResidentVertexStream !Buffer !Word32

uploadVertexStream :: Session -> VkCommandBuffer -> Word32 -> Int -> (Ptr () -> IO ()) -> IO ResidentVertexStream
uploadVertexStream session commands count stride write = mask_ $ do
  let bytes = fromIntegral count * fromIntegral stride
  buffer <- createBuffer session bytes (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
    VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  (withStaging session bytes $ \staging -> do
    write (mappedAddress staging)
    runUpload session commands $ do
      copyBuffer session commands (mappedBuffer staging) buffer 0 bytes
      bufferVisibility session commands buffer VK_PIPELINE_STAGE_VERTEX_INPUT_BIT VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT)
    `onException` destroyBuffer session buffer
  pure (ResidentVertexStream buffer count)

destroyVertexStream :: Session -> ResidentVertexStream -> IO ()
destroyVertexStream session (ResidentVertexStream buffer _) = destroyBuffer session buffer

pokeCorners :: Ptr () -> [Vec3] -> IO ()
pokeCorners destination corners = go destination corners
  where
    go _ [] = pure ()
    go ptr (Vec3 x y z : rest) = do
      pokeByteOff ptr 0 x
      pokeByteOff ptr 4 y
      pokeByteOff ptr 8 z
      go (ptr `plusPtr` cornerStride) rest

pokeInstances :: Ptr () -> [InstanceData] -> IO ()
pokeInstances destination records = go destination records
  where
    go _ [] = pure ()
    go ptr (InstanceData root parameters (r,g,b,a) tag : rest) = do
      pokeVec4 ptr instanceRootOffset root
      pokeVec4 ptr instanceParametersOffset parameters
      pokeByteOff ptr instanceColorOffset r
      pokeByteOff ptr (instanceColorOffset + 1) g
      pokeByteOff ptr (instanceColorOffset + 2) b
      pokeByteOff ptr (instanceColorOffset + 3) a
      pokeByteOff ptr instanceTagOffset tag
      go (ptr `plusPtr` instanceStride) rest
    pokeVec4 ptr offset (Vec4 x y z w) = do
      pokeByteOff ptr offset x
      pokeByteOff ptr (offset + 4) y
      pokeByteOff ptr (offset + 8) z
      pokeByteOff ptr (offset + 12) w

copyBuffer :: Session -> VkCommandBuffer -> Buffer -> Buffer -> Word64 -> Word64 -> IO ()
copyBuffer session commands source destination sourceOffset bytes =
  withStruct (structure [U64 sourceOffset, U64 0, U64 bytes]) $ \region ->
    vkCmdCopyBuffer (sessionFns session) commands (bufferHandle source) (bufferHandle destination) 1 region

withStaging :: Session -> Word64 -> (MappedBuffer -> IO a) -> IO a
withStaging session bytes =
  bracket (createMappedBuffer session bytes VK_BUFFER_USAGE_TRANSFER_SRC_BIT) (destroyMappedBuffer session)

pokeVertices :: Ptr () -> [Vertex] -> IO ()
pokeVertices destination vertices =
  case vertexLayout of
    [position, uv, color, normal, extra] -> go (attributeOffset position) (attributeOffset uv)
      (attributeOffset color) (attributeOffset normal) (attributeOffset extra) destination vertices
    _ -> throwIO (VulkanInvariantViolation "unexpected vertex layout")
  where
    go _ _ _ _ _ _ [] = pure ()
    go position uv color normal extra base (vertex : rest) = case vertexValues vertex of
      (Vec3 x y z, Vec2 u v, Color r g b a, Vec3 nx ny nz) -> do
        pokeByteOff base position x
        pokeByteOff base (position + 4) y
        pokeByteOff base (position + 8) z
        pokeByteOff base uv u
        pokeByteOff base (uv + 4) v
        pokeByteOff base color r
        pokeByteOff base (color + 4) g
        pokeByteOff base (color + 8) b
        pokeByteOff base (color + 12) a
        pokeByteOff base normal nx
        pokeByteOff base (normal + 4) ny
        pokeByteOff base (normal + 8) nz
        let Vec2 first second = vertexExtra vertex
        pokeByteOff base extra first
        pokeByteOff base (extra + 4) second
        go position uv color normal extra (base `plusPtr` vertexStride) rest
