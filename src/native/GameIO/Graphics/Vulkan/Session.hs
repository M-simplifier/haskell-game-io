{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module GameIO.Graphics.Vulkan.Session where

import Control.Exception (bracket, finally, mask_, throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef
import Data.List (find)
import Foreign hiding (void)
import Foreign.C.String (CString, peekCString, withCString)
import GameIO.Graphics.Shader (uniformDescriptorSet)
import GameIO.Graphics.Vulkan.Layout
import GameIO.Graphics.Vulkan.Lifetime
import GameIO.Graphics.Vulkan.Raw
import GameIO.Platform.Win32 (Window, retainNativeWindow, windowHandles)

data Scope = Scope !(IO Bool) !(IORef [IO ()])

newScope :: IO Bool -> IO Scope
newScope allowRelease = Scope allowRelease <$> newIORef []

withScope :: IO Bool -> (Scope -> IO a) -> IO a
withScope allowRelease = bracket (newScope allowRelease) closeScope

closeScope :: Scope -> IO ()
closeScope (Scope allowRelease releases) = mask_ $ do
  allowed <- allowRelease
  when allowed $ do
    actions <- atomicModifyIORef' releases (\pending -> ([], pending))
    foldr finally (pure ()) actions

own :: Scope -> IO a -> (a -> IO ()) -> IO a
own (Scope _ releases) create destroy = mask_ $ do
  resource <- create
  modifyIORef' releases (destroy resource :)
  pure resource

data Adapter = Adapter
  { adapterHandle :: !VkPhysicalDevice
  , adapterName :: !String
  , adapterQueueFamily :: !Word32
  , adapterMaintenanceExtension :: !String
  , adapterMemoryTypes :: ![Word32]
  , adapterMaximumImageDimension :: !Word32
  , adapterTimestampBits :: !Word32
  , adapterTimestampPeriodNSec :: !Float
  , adapterUniformAlignment :: !Word64
  }

data Session = Session
  { sessionAdapter :: !Adapter
  , sessionSurface :: !VkSurfaceKHR
  , sessionInstanceFns :: !InstanceFns
  , sessionDevice :: !VkDevice
  , sessionFns :: !DeviceFns
  , sessionQueue :: !VkQueue
  , sessionCleanup :: !CleanupGate
  }

withSession :: Window -> (Session -> IO a) -> IO a
withSession window use = do
  cleanup <- newCleanupGate (retainNativeWindow window)
  openSession window cleanup use

openSession :: Window -> CleanupGate -> (Session -> IO a) -> IO a
openSession window cleanup use = withVulkan (cleanupAllowed cleanup) $ \instanceAddress -> do
  global <- loadGlobalFns (instanceAddress nullPtr)
  version <- alloca $ \out -> do
    vkCheck "vkEnumerateInstanceVersion" (vkEnumerateInstanceVersion global out)
    peek out
  unless (version >= vulkan13) $
    throwIO (VulkanCapabilityUnavailable "Vulkan 1.3 loader required")
  extensions <- enumerateExtensions (vkEnumerateInstanceExtensionProperties global nullPtr)
  let required = ["VK_KHR_surface", "VK_KHR_win32_surface", "VK_KHR_get_surface_capabilities2"]
  unless (all (`elem` extensions) required) $
    throwIO (VulkanCapabilityUnavailable "Win32 surface extensions unavailable")
  let maintenance = filter ((`elem` extensions) . fst) maintenanceExtensions
  when (null maintenance) $
    throwIO (VulkanCapabilityUnavailable "No instance surface-maintenance extension")
  let enabled = required ++ map fst maintenance
  withScope (cleanupAllowed cleanup) $ \scope -> do
    instanceHandle <- own scope (createInstance global enabled) $ \handle -> do
      address <- instanceAddress handle "vkDestroyInstance"
      callDestroyInstance (castFunPtr address) handle nullPtr
    instanceFns <- loadInstanceFns (instanceAddress instanceHandle)
    surface <- own scope (createSurface window instanceFns instanceHandle)
      (\handle -> vkDestroySurfaceKHR instanceFns instanceHandle handle nullPtr)
    adapter <- pickAdapter instanceFns instanceHandle surface (map snd maintenance)
    device <- own scope (createDevice instanceFns adapter) $ \handle -> do
      address <- deviceAddress instanceFns handle "vkDestroyDevice"
      callDestroyDevice (castFunPtr address) handle nullPtr
    fns <- loadDeviceFns (deviceAddress instanceFns device)
    queue <- alloca $ \out -> vkGetDeviceQueue fns device (adapterQueueFamily adapter) 0 out >> peek out
    use (Session adapter surface instanceFns device fns queue cleanup)

createInstance :: GlobalFns -> [String] -> IO VkInstance
createInstance global extensions = withCString "Haskell Game IO" $ \name ->
  withNames extensions $ \count names ->
    withInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
      [pointer name, U32 0, pointer name, U32 0, U32 vulkan13] $ \application ->
        withInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
          [U32 0, pointer application, U32 0, pointer nullPtr, U32 count, pointer names] $ \create ->
            newHandle "vkCreateInstance" (vkCreateInstance global create nullPtr)

createSurface :: Window -> InstanceFns -> VkInstance -> IO VkSurfaceKHR
createSurface window fns instanceHandle =
  let (hinstance, hwnd) = windowHandles window
  in withInfo VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
      [U32 0, pointer hinstance, pointer hwnd] $ \create ->
        newHandle "vkCreateWin32SurfaceKHR" (vkCreateWin32SurfaceKHR fns instanceHandle create nullPtr)

createDevice :: InstanceFns -> Adapter -> IO VkDevice
createDevice fns adapter =
  withNames ["VK_KHR_swapchain", adapterMaintenanceExtension adapter] $ \count names ->
    with (1 :: Float) $ \priority ->
      withInfo VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        [U32 0, U32 (adapterQueueFamily adapter), U32 1, pointer priority] $ \queueInfo ->
          withInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR [U32 1] $ \maintenance ->
            withInfoNext VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES maintenance [U32 1] $ \rendering ->
              withInfoNext VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO rendering
                [U32 0, U32 1, pointer queueInfo, U32 0, pointer nullPtr, U32 count, pointer names, pointer nullPtr] $ \create ->
                  newHandle "vkCreateDevice" (vkCreateDevice fns (adapterHandle adapter) create nullPtr)

pickAdapter :: InstanceFns -> VkInstance -> VkSurfaceKHR -> [String] -> IO Adapter
pickAdapter fns instanceHandle surface maintenance = do
  devices <- enumerate "vkEnumeratePhysicalDevices" (Abi 8 8)
    (\count values -> vkEnumeratePhysicalDevices fns instanceHandle count (castPtr values))
    (peek . castPtr)
  choose [] devices
  where
    choose reasons [] = throwIO $ VulkanCapabilityUnavailable $
      "Vulkan 1.3 graphics/present queue, D32, dynamic rendering and swapchain maintenance required"
        ++ concatMap ("\n  " ++) (reverse reasons)
    choose reasons (physical : remaining) = do
      candidate <- inspectAdapter fns surface maintenance physical
      case candidate of
        Right adapter -> pure adapter
        Left reason -> choose (reason : reasons) remaining

inspectAdapter :: InstanceFns -> VkSurfaceKHR -> [String] -> VkPhysicalDevice -> IO (Either String Adapter)
inspectAdapter fns surface maintenance physical =
  withOutput physicalDevicePropertiesAbi $ \properties -> do
    vkGetPhysicalDeviceProperties fns physical properties
    version <- peekByteOff properties physicalDeviceProperties_apiVersion :: IO Word32
    name <- peekCString (properties `plusPtr` physicalDeviceProperties_deviceName)
    let reject reason = pure (Left (name ++ ": " ++ reason))
    maxSets <- peekByteOff properties physicalDeviceProperties_maxBoundDescriptorSets :: IO Word32
    if version < vulkan13
      then reject "Vulkan 1.3 unavailable"
      else if maxSets <= uniformDescriptorSet
      then reject "five bound descriptor sets unavailable"
      else do
        extensions <- enumerateExtensions (vkEnumerateDeviceExtensionProperties fns physical nullPtr)
        case find (`elem` extensions) maintenance of
          Nothing -> reject "swapchain maintenance unavailable"
          Just extension
            | "VK_KHR_swapchain" `notElem` extensions -> reject "swapchain unavailable"
            | otherwise -> do
                supported <- checkFeatures fns physical
                depth <- supportsDepth fns physical
                queue <- pickQueue fns physical surface
                case queue of
                  Nothing -> reject "no queue supports both graphics and this Win32 surface"
                  Just (family, timestampBits)
                    | not supported -> reject "dynamic rendering or swapchain maintenance feature disabled"
                    | not depth -> reject "D32_SFLOAT depth attachment unavailable"
                    | otherwise -> do
                        memory <- readMemoryTypes fns physical
                        limit <- peekByteOff properties physicalDeviceProperties_maxImageDimension2D
                        timestampPeriod <- peekByteOff properties physicalDeviceProperties_timestampPeriod
                        uniformAlignment <- peekByteOff properties physicalDeviceProperties_minUniformBufferOffsetAlignment
                        pure (Right (Adapter physical name family extension memory limit timestampBits timestampPeriod uniformAlignment))

checkFeatures :: InstanceFns -> VkPhysicalDevice -> IO Bool
checkFeatures fns physical =
  withInfo VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR [U32 0] $ \maintenance ->
    withInfoNext VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES maintenance [U32 0] $ \rendering ->
      withOutput physicalDeviceFeatures2Abi $ \features -> do
        pokeByteOff features 0 VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
        pokeByteOff features 8 rendering
        vkGetPhysicalDeviceFeatures2 fns physical features
        canRender <- peekByteOff rendering physicalDeviceDynamicRenderingFeatures_dynamicRendering :: IO Word32
        canMaintain <- peekByteOff maintenance physicalDeviceSwapchainMaintenance1FeaturesKHR_swapchainMaintenance1 :: IO Word32
        pure (canRender /= 0 && canMaintain /= 0)

supportsDepth :: InstanceFns -> VkPhysicalDevice -> IO Bool
supportsDepth fns physical =
  supportsFormat fns physical VK_FORMAT_D32_SFLOAT VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

supportsFormat :: InstanceFns -> VkPhysicalDevice -> Word32 -> Word32 -> IO Bool
supportsFormat fns physical format required = withOutput formatPropertiesAbi $ \properties -> do
  vkGetPhysicalDeviceFormatProperties fns physical format properties
  flags <- peekByteOff properties formatProperties_optimalTilingFeatures
  pure (flags .&. required == required)

pickQueue :: InstanceFns -> VkPhysicalDevice -> VkSurfaceKHR -> IO (Maybe (Word32, Word32))
pickQueue fns physical surface = alloca $ \count -> do
  vkGetPhysicalDeviceQueueFamilyProperties fns physical count nullPtr
  families <- peek count
  if families == 0 then pure Nothing else
    withOutput (queueFamilyPropertiesAbi { abiSize = fromIntegral families * abiSize queueFamilyPropertiesAbi }) $ \properties -> do
      vkGetPhysicalDeviceQueueFamilyProperties fns physical count properties
      written <- peek count
      go properties 0 written
  where
    go properties family count
      | family >= count = pure Nothing
      | otherwise = do
          let queue = properties `plusPtr` (fromIntegral family * abiSize queueFamilyPropertiesAbi)
          flags <- peekByteOff queue queueFamilyProperties_queueFlags
          queueCount <- peekByteOff queue queueFamilyProperties_queueCount :: IO Word32
          if flags .&. VK_QUEUE_GRAPHICS_BIT == 0 || queueCount == 0
            then go properties (family + 1) count
            else alloca $ \supported -> do
              vkCheck "vkGetPhysicalDeviceSurfaceSupportKHR"
                (vkGetPhysicalDeviceSurfaceSupportKHR fns physical family surface supported)
              canPresent <- peek supported
              if canPresent == 0
                then go properties (family + 1) count
                else do
                  bits <- peekByteOff queue queueFamilyProperties_timestampValidBits
                  pure (Just (family, bits))

readMemoryTypes :: InstanceFns -> VkPhysicalDevice -> IO [Word32]
readMemoryTypes fns physical = withOutput physicalDeviceMemoryPropertiesAbi $ \properties -> do
  vkGetPhysicalDeviceMemoryProperties fns physical properties
  count <- peekByteOff properties physicalDeviceMemoryProperties_memoryTypeCount :: IO Word32
  when (count == 0 || count > maximumMemoryTypes) $
    throwIO (VulkanInvariantViolation "invalid Vulkan memory type count")
  forM [0 .. fromIntegral count - 1] $ \i ->
    peekByteOff properties
      (physicalDeviceMemoryProperties_memoryTypes + i * abiSize memoryTypeAbi + memoryType_propertyFlags)

memoryType :: Session -> Word32 -> Word32 -> IO Word32
memoryType session allowed required =
  case find usable (zip [0..] (adapterMemoryTypes (sessionAdapter session))) of
    Just (index, _) -> pure (fromIntegral (index :: Int))
    Nothing -> throwIO (VulkanCapabilityUnavailable ("no memory type supports flags " ++ show required))
  where
    usable (index, flags) = allowed .&. bit index /= 0 && flags .&. required == required

data SurfaceConfig = SurfaceConfig
  { surfaceWidth :: !Word32
  , surfaceHeight :: !Word32
  , surfaceFormat :: !Word32
  , surfaceColorSpace :: !Word32
  , surfaceImages :: !Word32
  , surfaceTransform :: !Word32
  , surfaceComposite :: !Word32
  , surfaceUsage :: !Word32
  } deriving (Eq, Show)

surfaceConfig :: Session -> (Int, Int) -> IO (Maybe SurfaceConfig)
surfaceConfig session (desiredWidth, desiredHeight)
  | desiredWidth <= 0 || desiredHeight <= 0 = pure Nothing
  | otherwise = withOutput surfaceCapabilitiesKHRAbi $ \properties -> do
      vkCheck "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
        (vkGetPhysicalDeviceSurfaceCapabilitiesKHR fns physical surface properties)
      let dimension fieldOffset = peekByteOff properties fieldOffset
      minImages <- dimension surfaceCapabilitiesKHR_minImageCount
      maxImages <- dimension surfaceCapabilitiesKHR_maxImageCount
      currentWidth <- dimension surfaceCapabilitiesKHR_currentExtent
      currentHeight <- dimension (surfaceCapabilitiesKHR_currentExtent + 4)
      minWidth <- dimension surfaceCapabilitiesKHR_minImageExtent
      minHeight <- dimension (surfaceCapabilitiesKHR_minImageExtent + 4)
      maxWidth <- dimension surfaceCapabilitiesKHR_maxImageExtent
      maxHeight <- dimension (surfaceCapabilitiesKHR_maxImageExtent + 4)
      transform <- dimension surfaceCapabilitiesKHR_currentTransform
      composite <- dimension surfaceCapabilitiesKHR_supportedCompositeAlpha
      usage <- dimension surfaceCapabilitiesKHR_supportedUsageFlags
      let width = chooseExtent currentWidth minWidth maxWidth desiredWidth
      let height = chooseExtent currentHeight minHeight maxHeight desiredHeight
      if width == 0 || height == 0 then pure Nothing else do
        when (usage .&. VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT == 0 || composite == 0) $
          throwIO (VulkanCapabilityUnavailable "surface does not support color attachment presentation")
        (format, colorSpace) <- chooseFormat fns physical surface
        let imageCount = if maxImages == 0 then minImages + 1 else min maxImages (minImages + 1)
        let compositeMode = if composite .&. VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR /= 0
              then VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR else bit (countTrailingZeros composite)
        let used = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. (usage .&. VK_IMAGE_USAGE_TRANSFER_SRC_BIT)
        pure $ Just (SurfaceConfig width height format colorSpace imageCount transform compositeMode used)
  where
    fns = sessionInstanceFns session
    physical = adapterHandle (sessionAdapter session)
    surface = sessionSurface session
    chooseExtent current lo hi desired
      | current == maxBound = min hi (max lo (fromIntegral desired))
      | otherwise = current

chooseFormat :: InstanceFns -> VkPhysicalDevice -> VkSurfaceKHR -> IO (Word32, Word32)
chooseFormat fns physical surface = do
  formats <- enumerate "vkGetPhysicalDeviceSurfaceFormatsKHR" surfaceFormatKHRAbi
    (vkGetPhysicalDeviceSurfaceFormatsKHR fns physical surface) $ \out ->
      (,) <$> peekByteOff out surfaceFormatKHR_format <*> peekByteOff out surfaceFormatKHR_colorSpace
  -- Presentation shaders that output encoded RGB (such as the tone-map pass)
  -- need a UNORM destination so the driver does not apply a second transfer.
  let ordered = [VK_FORMAT_B8G8R8A8_UNORM, VK_FORMAT_R8G8B8A8_UNORM]
  let candidates = [(format, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) | format <- ordered]
  case formats of
    [(VK_FORMAT_UNDEFINED, colorSpace)] -> pure (VK_FORMAT_B8G8R8A8_UNORM, colorSpace)
    _ -> case find (`elem` formats) candidates of
      Just format -> pure format
      Nothing -> throwIO (VulkanCapabilityUnavailable "no supported 32-bit RGBA/BGRA UNORM surface format")

data Swapchain = Swapchain
  { swapchainHandle :: !VkSwapchainKHR
  , swapchainImages :: ![PresentImage]
  }

data PresentImage = PresentImage
  { presentImage :: !VkImage
  , presentView :: !VkImageView
  , presentSemaphore :: !VkSemaphore
  , presentFence :: !VkFence
  , presentPending :: !(IORef Bool)
  }

createSwapchain :: Scope -> Session -> SurfaceConfig -> IO Swapchain
createSwapchain scope session config = do
  swapchain <- own scope create (\handle -> vkDestroySwapchainKHR fns device handle nullPtr)
  images <- enumerate "vkGetSwapchainImagesKHR" (Abi 8 8)
    (\count out -> vkGetSwapchainImagesKHR fns device swapchain count (castPtr out)) (peek . castPtr)
  when (null images) (throwIO (VulkanInvariantViolation "Vulkan returned an empty swapchain"))
  owned <- forM images $ \image -> do
    view <- own scope (createImageView session image (surfaceFormat config) VK_IMAGE_ASPECT_COLOR_BIT)
      (\handle -> vkDestroyImageView fns device handle nullPtr)
    ready <- own scope (createSemaphore session) (\handle -> vkDestroySemaphore fns device handle nullPtr)
    finished <- own scope (createFence session False) (\handle -> vkDestroyFence fns device handle nullPtr)
    PresentImage image view ready finished <$> newIORef False
  pure (Swapchain swapchain owned)
  where
    fns = sessionFns session
    device = sessionDevice session
    create = withInfo VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
      [ U32 0, pointer (sessionSurface session), U32 (surfaceImages config)
      , U32 (surfaceFormat config), U32 (surfaceColorSpace config)
      , extent2D (surfaceWidth config) (surfaceHeight config)
      , U32 1, U32 (surfaceUsage config), U32 VK_SHARING_MODE_EXCLUSIVE
      , U32 0, pointer nullPtr, U32 (surfaceTransform config), U32 (surfaceComposite config)
      , U32 VK_PRESENT_MODE_FIFO_KHR, U32 1, pointer nullPtr
      ] $ \infoPtr -> newHandle "vkCreateSwapchainKHR" (vkCreateSwapchainKHR fns device infoPtr nullPtr)

createImageView :: Session -> VkImage -> Word32 -> Word32 -> IO VkImageView
createImageView session image format aspect =
  withInfo VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
    [U32 0, pointer image, U32 VK_IMAGE_VIEW_TYPE_2D, U32 format
    , words32 (replicate 4 VK_COMPONENT_SWIZZLE_IDENTITY)
    , imageRange aspect
    ] $ \infoPtr -> newHandle "vkCreateImageView"
      (vkCreateImageView (sessionFns session) (sessionDevice session) infoPtr nullPtr)

imageRange :: Word32 -> Field
imageRange aspect = words32 [aspect, 0, 1, 0, 1]

createSemaphore :: Session -> IO VkSemaphore
createSemaphore session = withInfo VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO [U32 0] $ \infoPtr ->
  newHandle "vkCreateSemaphore" (vkCreateSemaphore (sessionFns session) (sessionDevice session) infoPtr nullPtr)

createFence :: Session -> Bool -> IO VkFence
createFence session signalled =
  withInfo VK_STRUCTURE_TYPE_FENCE_CREATE_INFO [U32 (if signalled then VK_FENCE_CREATE_SIGNALED_BIT else 0)] $ \infoPtr ->
    newHandle "vkCreateFence" (vkCreateFence (sessionFns session) (sessionDevice session) infoPtr nullPtr)

data CompletionSource = QueueSubmission | WindowSystemOperation

waitFence :: Session -> CompletionSource -> VkFence -> IO ()
waitFence session source fence = requireCompletion (sessionCleanup session) wait recover
  where
    fns = sessionFns session
    device = sessionDevice session
    wait = with fence $ \one ->
      vkCheck "vkWaitForFences" (vkWaitForFences fns device 1 one 1 maxBound)
    recover = case source of
      QueueSubmission -> vkCheck "vkDeviceWaitIdle" (vkDeviceWaitIdle fns device)
      WindowSystemOperation -> wait

resetFence :: Session -> VkFence -> IO ()
resetFence session fence = with fence $ \one ->
  vkCheck "vkResetFences" (vkResetFences (sessionFns session) (sessionDevice session) 1 one)

waitPresentation :: Session -> PresentImage -> IO ()
waitPresentation session image = do
  pending <- readIORef (presentPending image)
  when pending $ do
    waitFence session WindowSystemOperation (presentFence image)
    writeIORef (presentPending image) False

waitSwapchain :: Session -> Swapchain -> IO ()
waitSwapchain session swapchain = forM_ (swapchainImages swapchain) (waitPresentation session)

present :: Session -> Swapchain -> PresentImage -> Word32 -> IO Int32
present session swapchain image index = mask_ $ do
  resetFence session (presentFence image)
  with (presentFence image) $ \fences ->
    withInfo VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR [U32 1, pointer fences] $ \fenceInfo ->
      with (presentSemaphore image) $ \waits ->
        with (swapchainHandle swapchain) $ \swapchains ->
          with index $ \indices ->
            withInfoNext VK_STRUCTURE_TYPE_PRESENT_INFO_KHR fenceInfo
              [U32 1, pointer waits, U32 1, pointer swapchains, pointer indices, pointer nullPtr] $ \presentInfo -> do
                result <- vkQueuePresentKHR (sessionFns session) (sessionQueue session) presentInfo
                -- These results enqueue the operation even if the surface rejects it.
                when (result `elem` [VK_SUCCESS, VK_SUBOPTIMAL_KHR, VK_ERROR_OUT_OF_DATE_KHR, VK_ERROR_SURFACE_LOST_KHR]) $
                  writeIORef (presentPending image) True
                pure result

newHandle :: String -> (Ptr (Ptr a) -> IO Int32) -> IO (Ptr a)
newHandle label create = alloca $ \out -> do
  poke out nullPtr
  vkCheck label (create out)
  handle <- peek out
  when (handle == nullPtr) (throwIO (VulkanInvariantViolation (label ++ " returned a null handle")))
  pure handle

withNames :: [String] -> (Word32 -> Ptr CString -> IO a) -> IO a
withNames names use = withMany withCString names $ \encoded ->
  withArray encoded (use (fromIntegral (length names)))

enumerateExtensions :: (Ptr Word32 -> Ptr () -> IO Int32) -> IO [String]
enumerateExtensions run = enumerate "Vulkan extension enumeration" extensionPropertiesAbi run $ \entry ->
  peekCString (entry `plusPtr` extensionProperties_extensionName)

enumerate :: String -> Abi -> (Ptr Word32 -> Ptr () -> IO Int32) -> (Ptr () -> IO a) -> IO [a]
enumerate label abi run readValue = alloca $ \outCount -> do
  vkCheck label (run outCount nullPtr)
  count <- peek outCount
  if count == 0 then pure [] else do
    attempt <- withOutput (abi { abiSize = fromIntegral count * abiSize abi }) $ \values -> do
      result <- run outCount values
      if result == VK_INCOMPLETE
        then pure Nothing
        else do
          vkCheck label (pure result)
          written <- peek outCount
          when (written > count) (throwIO (VulkanInvariantViolation (label ++ " wrote too many records")))
          Just <$> forM [0 .. fromIntegral written - 1] (\i -> readValue (values `plusPtr` (i * abiSize abi)))
    maybe (enumerate label abi run readValue) pure attempt

maintenanceExtensions :: [(String, String)]
maintenanceExtensions =
  [ ("VK_KHR_surface_maintenance1", "VK_KHR_swapchain_maintenance1")
  , ("VK_EXT_surface_maintenance1", "VK_EXT_swapchain_maintenance1")
  ]

vulkan13 :: Word32
vulkan13 = (1 `shiftL` 22) .|. (3 `shiftL` 12)

maximumMemoryTypes :: Word32
maximumMemoryTypes = 32
