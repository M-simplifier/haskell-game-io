{-# LANGUAGE PatternSynonyms #-}

module GameIO.Graphics.Vulkan.Pipeline where

import Control.Exception (bracket, finally, mask_, onException, throwIO)
import Control.Monad (when)
import Foreign
import Foreign.C.String (withCString)
import GameIO.Graphics.Plan
import GameIO.Graphics.Shader
import GameIO.Graphics.Vulkan.Layout
import GameIO.Graphics.Vulkan.Lifetime
import GameIO.Graphics.Vulkan.Raw
import GameIO.Graphics.Vulkan.Resources
import GameIO.Graphics.Vulkan.Session

data TextureFilter = Nearest | Linear deriving (Eq, Ord, Show)

data Bindings = Bindings
  { descriptorLayout :: !VkDescriptorSetLayout
  , parameterLayout :: !VkDescriptorSetLayout
  , descriptorPool :: !VkDescriptorPool
  , pipelineLayout :: !VkPipelineLayout
  , nearestSampler :: !VkSampler
  , linearSampler :: !VkSampler
  }

data Pipelines = Pipelines
  { worldPipeline :: !VkPipeline
  , overlayPipeline :: !VkPipeline
  }

createBindings :: Scope -> Session -> IO Bindings
createBindings scope session = do
  layout <- own scope (createDescriptorLayout VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT))
    (\handle -> vkDestroyDescriptorSetLayout fns device handle nullPtr)
  parameters <- own scope (createDescriptorLayout VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
    (VK_SHADER_STAGE_VERTEX_BIT .|. VK_SHADER_STAGE_FRAGMENT_BIT))
    (\handle -> vkDestroyDescriptorSetLayout fns device handle nullPtr)
  pool <- own scope createPool (\handle -> vkDestroyDescriptorPool fns device handle nullPtr)
  pipeline <- own scope (createLayout layout parameters) (\handle -> vkDestroyPipelineLayout fns device handle nullPtr)
  nearest <- own scope (createSampler VK_FILTER_NEAREST) (\handle -> vkDestroySampler fns device handle nullPtr)
  linear <- own scope (createSampler VK_FILTER_LINEAR) (\handle -> vkDestroySampler fns device handle nullPtr)
  pure (Bindings layout parameters pool pipeline nearest linear)
  where
    fns = sessionFns session
    device = sessionDevice session
    createDescriptorLayout descriptorType shaderStages =
      withStruct (structure
        [ U32 textureBinding, U32 descriptorType, U32 1, U32 shaderStages, pointer nullPtr
        ]) $ \binding ->
          withInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
            [U32 0, U32 1, pointer binding] $ \create ->
              newHandle "vkCreateDescriptorSetLayout" (vkCreateDescriptorSetLayout fns device create nullPtr)
    createPool =
      withStructArray (structure [U32 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, U32 textureCapacity])
        [structure [U32 VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, U32 16]] $ \sizes ->
        withInfo VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
          [U32 VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT, U32 (textureCapacity + 16), U32 2, pointer sizes] $ \create ->
            newHandle "vkCreateDescriptorPool" (vkCreateDescriptorPool fns device create nullPtr)
    createLayout setLayout uniforms =
      withArray (replicate (fromIntegral uniformDescriptorSet) setLayout ++ [uniforms]) $ \layouts ->
        withStruct (structure [U32 VK_SHADER_STAGE_VERTEX_BIT, U32 transformOffset, U32 transformBytes]) $ \pushRange ->
          withInfo VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
            [U32 0, U32 (uniformDescriptorSet + 1), pointer layouts, U32 1, pointer pushRange] $ \create ->
              newHandle "vkCreatePipelineLayout" (vkCreatePipelineLayout fns device create nullPtr)
    createSampler filterMode = withInfo VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
      [ U32 0, U32 filterMode, U32 filterMode, U32 VK_SAMPLER_MIPMAP_MODE_NEAREST
      , U32 VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
      , U32 VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
      , U32 VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
      , F32 0, U32 0, F32 1, U32 0, U32 VK_COMPARE_OP_ALWAYS, F32 0, F32 0
      , U32 VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK, U32 0
      ] $ \create -> newHandle "vkCreateSampler" (vkCreateSampler fns device create nullPtr)

textureCapacity :: Word32
textureCapacity = 4096

data ResidentTexture = ResidentTexture
  { residentTextureImage :: !ImageWithView
  , residentDescriptor :: !VkDescriptorSet
  }

describeTexture :: Session -> Bindings -> TextureFilter -> ImageWithView -> IO ResidentTexture
describeTexture session bindings filterMode image = mask_ $ do
  descriptor <- allocateDescriptor session bindings (descriptorLayout bindings)
  let texture = ResidentTexture image descriptor
  let sampler = case filterMode of Nearest -> nearestSampler bindings; Linear -> linearSampler bindings
  (withStruct (structure
    [pointer sampler, pointer (ownedImageView image), U32 VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL]) $ \imageInfo ->
      withInfo VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
        [ pointer descriptor, U32 textureBinding, U32 0, U32 1, U32 VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , pointer imageInfo, pointer nullPtr, pointer nullPtr
        ] $ \write -> vkUpdateDescriptorSets fns device 1 write 0 nullPtr)
    `onException` freeDescriptor session bindings descriptor
  pure texture
  where
    fns = sessionFns session
    device = sessionDevice session

destroyResidentTexture :: Session -> Bindings -> ResidentTexture -> IO ()
destroyResidentTexture session bindings texture = releaseWhenAllowed (sessionCleanup session) $
  freeDescriptor session bindings (residentDescriptor texture)
    `finally` destroyImageWithView session (residentTextureImage texture)

freeDescriptor :: Session -> Bindings -> VkDescriptorSet -> IO ()
freeDescriptor session bindings descriptor = with descriptor $ \descriptors ->
  vkCheck "vkFreeDescriptorSets" $
    vkFreeDescriptorSets (sessionFns session) (sessionDevice session) (descriptorPool bindings) 1 descriptors

allocateDescriptor :: Session -> Bindings -> VkDescriptorSetLayout -> IO VkDescriptorSet
allocateDescriptor session bindings layout = with layout $ \layouts ->
  withInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
    [pointer (descriptorPool bindings), U32 1, pointer layouts] $ \allocate ->
      newHandle "vkAllocateDescriptorSets" (vkAllocateDescriptorSets (sessionFns session) (sessionDevice session) allocate)

describeParameters :: Session -> Bindings -> Buffer -> IO VkDescriptorSet
describeParameters session bindings buffer = mask_ $ do
  descriptor <- allocateDescriptor session bindings (parameterLayout bindings)
  (withStruct (structure [pointer (bufferHandle buffer), U64 0, U64 (fromIntegral uniformBytes)]) $ \dataInfo ->
    withInfo VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
      [ pointer descriptor, U32 0, U32 0, U32 1, U32 VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
      , pointer nullPtr, pointer dataInfo, pointer nullPtr
      ] $ \write -> vkUpdateDescriptorSets (sessionFns session) (sessionDevice session) 1 write 0 nullPtr)
    `onException` freeDescriptor session bindings descriptor
  pure descriptor

data PipelineMode = WorldDepth | WorldAlpha | WorldAdditive | OverlayBlend | OverlayReplace
  deriving (Eq, Ord, Show)

data VertexInput = MeshVertices | InstancedCorners deriving (Eq, Enum)

createPipelines :: Scope -> Session -> Bindings -> Word32 -> IO Pipelines
createPipelines scope session bindings format =
  withShaderStages session vertexWords fragmentWords $ \stages ->
    Pipelines
      <$> own scope (createPipeline session bindings format stages WorldDepth MeshVertices) (destroyPipeline session)
      <*> own scope (createPipeline session bindings format stages OverlayBlend MeshVertices) (destroyPipeline session)

createMaterialPipeline :: Scope -> Session -> Bindings -> Word32 -> PipelineMode -> VertexInput
                       -> [Word32] -> [Word32] -> IO VkPipeline
createMaterialPipeline scope session bindings format mode vertexInput vertex fragment =
  withShaderStages session vertex fragment $ \stages ->
    own scope (createPipeline session bindings format stages mode vertexInput) (destroyPipeline session)

destroyPipeline :: Session -> VkPipeline -> IO ()
destroyPipeline session pipeline = vkDestroyPipeline (sessionFns session) (sessionDevice session) pipeline nullPtr

withShaderStages :: Session -> [Word32] -> [Word32] -> (Ptr () -> IO a) -> IO a
withShaderStages session vertex fragment use =
  withShader session vertex $ \vertexShader ->
    withShader session fragment $ \fragmentShader ->
      withCString "main" $ \entry -> do
        let stage kind shader = info VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO nullPtr
              [U32 0, U32 kind, pointer shader, pointer entry, pointer nullPtr]
        withStructArray (stage VK_SHADER_STAGE_VERTEX_BIT vertexShader)
          [stage VK_SHADER_STAGE_FRAGMENT_BIT fragmentShader] use

vertexWords, fragmentWords :: [Word32]
vertexWords = compileVertexShader defaultVertexShader
fragmentWords = compileFragmentShader defaultFragmentShader

withShader :: Session -> [Word32] -> (VkShaderModule -> IO a) -> IO a
withShader session shaderWords = bracket create destroy
  where
    fns = sessionFns session
    device = sessionDevice session
    create = withArrayLen shaderWords $ \count wordsPtr ->
      withInfo VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
        [U32 0, U64 (fromIntegral count * 4), pointer wordsPtr] $ \shaderInfo ->
          newHandle "vkCreateShaderModule" (vkCreateShaderModule fns device shaderInfo nullPtr)
    destroy shader = vkDestroyShaderModule fns device shader nullPtr

createPipeline :: Session -> Bindings -> Word32 -> Ptr () -> PipelineMode -> VertexInput -> IO VkPipeline
createPipeline session bindings colorFormat stages mode vertexInput =
  with colorFormat $ \formats ->
    withInfo VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO
      [U32 0, U32 1, pointer formats, U32 VK_FORMAT_D32_SFLOAT, U32 VK_FORMAT_UNDEFINED] $ \rendering ->
        withVertexInput vertexInput $ \input ->
          withInfo VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
            [U32 0, U32 VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, U32 0] $ \assembly ->
              withInfo VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
                [U32 0, U32 1, pointer nullPtr, U32 1, pointer nullPtr] $ \viewport ->
                  withRasterizer $ \raster ->
                    withMultisample $ \multisample ->
                      withDepth mode $ \depth ->
                        withBlend mode $ \blend ->
                          withArray [VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR] $ \states ->
                            withInfo VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
                              [U32 0, U32 2, pointer states] $ \dynamic ->
                                withInfoNext VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO rendering
                                  [ U32 0, U32 2, pointer stages, pointer input, pointer assembly, pointer nullPtr
                                  , pointer viewport, pointer raster, pointer multisample, pointer depth
                                  , pointer blend, pointer dynamic, pointer (pipelineLayout bindings)
                                  , pointer nullPtr, U32 0, pointer nullPtr, I32 (-1)
                                  ] $ \create -> createOne create
  where
    fns = sessionFns session
    device = sessionDevice session
    createOne create = alloca $ \out -> do
      poke out nullPtr
      result <- vkCreateGraphicsPipelines fns device nullPtr 1 create nullPtr out
      pipeline <- peek out
      when (result /= VK_SUCCESS && pipeline /= nullPtr) $
        vkDestroyPipeline fns device pipeline nullPtr
      vkCheck "vkCreateGraphicsPipelines" (pure result)
      when (pipeline == nullPtr) (throwIO (VulkanInvariantViolation "pipeline creation returned a null handle"))
      pure pipeline

withVertexInput :: VertexInput -> (Ptr () -> IO a) -> IO a
withVertexInput vertexInput use = do
  attributes <- case vertexInput of
    MeshVertices -> traverse meshAttribute vertexLayout
    InstancedCorners -> pure
      [ attribute positionLocation 0 VK_FORMAT_R32G32B32_SFLOAT 0
      , attribute instanceRootLocation 1 VK_FORMAT_R32G32B32A32_SFLOAT instanceRootOffset
      , attribute instanceParametersLocation 1 VK_FORMAT_R32G32B32A32_SFLOAT instanceParametersOffset
      , attribute instanceColorLocation 1 VK_FORMAT_R8G8B8A8_UNORM instanceColorOffset
      ]
  case attributes of
    [] -> throwIO (VulkanInvariantViolation "empty vertex layout")
    first : rest -> withStructArray first rest $ \attributeData ->
      withStructArray (perVertex (if vertexInput == MeshVertices then vertexStride else cornerStride)) instanceBinding $ \binding ->
        withInfo VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
          [U32 0, U32 (1 + fromIntegral (length instanceBinding)), pointer binding,
           U32 (fromIntegral (length attributes)), pointer attributeData] use
  where
    perVertex stride = structure [U32 0, U32 (fromIntegral stride), U32 VK_VERTEX_INPUT_RATE_VERTEX]
    instanceBinding = case vertexInput of
      MeshVertices -> []
      InstancedCorners -> [structure [U32 1, U32 (fromIntegral instanceStride), U32 VK_VERTEX_INPUT_RATE_INSTANCE]]
    attribute :: Word32 -> Word32 -> Word32 -> Int -> Structure
    attribute locationNumber binding format offset =
      structure [U32 locationNumber, U32 binding, U32 format, U32 (fromIntegral offset)]
    meshAttribute input = do
      format <- case attributeComponents input of
        2 -> pure VK_FORMAT_R32G32_SFLOAT
        3 -> pure VK_FORMAT_R32G32B32_SFLOAT
        4 -> pure VK_FORMAT_R32G32B32A32_SFLOAT
        _ -> throwIO (VulkanInvariantViolation "unsupported vertex attribute shape")
      pure (attribute (attributeLocation input) 0 format (attributeOffset input))

withRasterizer :: (Ptr () -> IO a) -> IO a
withRasterizer = withInfo VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  [ U32 0, U32 0, U32 0, U32 VK_POLYGON_MODE_FILL, U32 VK_CULL_MODE_NONE
  , U32 VK_FRONT_FACE_COUNTER_CLOCKWISE, U32 0, F32 0, F32 0, F32 0, F32 1
  ]

withMultisample :: (Ptr () -> IO a) -> IO a
withMultisample = withInfo VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  [U32 0, U32 VK_SAMPLE_COUNT_1_BIT, U32 0, F32 0, pointer nullPtr, U32 0, U32 0]

withDepth :: PipelineMode -> (Ptr () -> IO a) -> IO a
withDepth mode = withInfo VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  [U32 0, U32 enabled, U32 writeDepth, U32 VK_COMPARE_OP_LESS, U32 0, U32 0
  , words32 (replicate 7 0), words32 (replicate 7 0), F32 0, F32 1
  ]
  where
    enabled = if mode `elem` [WorldDepth, WorldAlpha, WorldAdditive] then 1 else 0
    writeDepth = if mode == WorldDepth then 1 else 0

withBlend :: PipelineMode -> (Ptr () -> IO a) -> IO a
withBlend mode use = withStruct (structure
  [ U32 enabled, U32 VK_BLEND_FACTOR_SRC_ALPHA, U32 destinationFactor, U32 VK_BLEND_OP_ADD
  , U32 VK_BLEND_FACTOR_ONE, U32 destinationFactor, U32 VK_BLEND_OP_ADD
  , U32 (VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT)
  ]) $ \attachment ->
    withInfo VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
      [U32 0, U32 0, U32 0, U32 1, pointer attachment, floats [0, 0, 0, 0]] use
  where
    enabled = if mode `elem` [OverlayBlend, WorldAlpha, WorldAdditive] then 1 else 0
    destinationFactor = if mode == WorldAdditive then VK_BLEND_FACTOR_ONE else VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA

setTransform :: Session -> Bindings -> VkCommandBuffer -> Mat4 -> IO ()
setTransform session bindings commands matrix =
  withArray (concatMap components (mat4Columns matrix)) $ \values ->
    vkCmdPushConstants (sessionFns session) commands (pipelineLayout bindings)
      VK_SHADER_STAGE_VERTEX_BIT transformOffset transformBytes (castPtr values)
  where
    components (Vec4 x y z w) = [x, y, z, w]

bindTexture :: Session -> Bindings -> VkCommandBuffer -> ResidentTexture -> IO ()
bindTexture session bindings commands texture = with (residentDescriptor texture) $ \descriptors ->
  vkCmdBindDescriptorSets (sessionFns session) commands VK_PIPELINE_BIND_POINT_GRAPHICS
    (pipelineLayout bindings) textureSet 1 descriptors 0 nullPtr

bindMaterialInputs :: Session -> Bindings -> VkCommandBuffer -> [ResidentTexture] -> VkDescriptorSet -> Word32 -> IO ()
bindMaterialInputs session bindings commands textures parameters offset =
  withArrayLen (map residentDescriptor textures) $ \count descriptors ->
    with parameters $ \parameterSet ->
      with offset $ \offsets -> do
        let bind = vkCmdBindDescriptorSets (sessionFns session) commands VK_PIPELINE_BIND_POINT_GRAPHICS (pipelineLayout bindings)
        bind textureSet (fromIntegral count) descriptors 0 nullPtr
        bind uniformDescriptorSet 1 parameterSet 1 offsets

bindVertices :: Session -> VkCommandBuffer -> Buffer -> IO ()
bindVertices session commands vertex = with (bufferHandle vertex) $ \buffers ->
  with (0 :: Word64) $ \offsets ->
    vkCmdBindVertexBuffers (sessionFns session) commands 0 1 buffers offsets
