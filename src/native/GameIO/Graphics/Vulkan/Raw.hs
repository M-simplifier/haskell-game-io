{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

-- Win64 subset of the Vulkan C ABI.
-- Signatures and enumerants follow Vulkan-Headers v1.4.341, vulkan_core.h and
-- vulkan_win32.h. All dispatchable and non-dispatchable handles are pointers.
-- https://github.com/KhronosGroup/Vulkan-Headers/tree/v1.4.341/include/vulkan
module GameIO.Graphics.Vulkan.Raw where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (when)
import Foreign
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CSize(..))

data VulkanError
  = VulkanLoaderUnavailable !Word32
  | VulkanFunctionUnavailable !String
  | VulkanCallFailed !String !Int32
  | VulkanCapabilityUnavailable !String
  | VulkanCompletionUnconfirmed
  | VulkanInvariantViolation !String
  deriving (Eq, Show)

instance Exception VulkanError

data VkBufferTag
type VkBuffer = Ptr VkBufferTag

data VkCommandBufferTag
type VkCommandBuffer = Ptr VkCommandBufferTag

data VkCommandPoolTag
type VkCommandPool = Ptr VkCommandPoolTag

data VkDescriptorPoolTag
type VkDescriptorPool = Ptr VkDescriptorPoolTag

data VkDescriptorSetTag
type VkDescriptorSet = Ptr VkDescriptorSetTag

data VkDescriptorSetLayoutTag
type VkDescriptorSetLayout = Ptr VkDescriptorSetLayoutTag

data VkDeviceTag
type VkDevice = Ptr VkDeviceTag

data VkDeviceMemoryTag
type VkDeviceMemory = Ptr VkDeviceMemoryTag

data VkFenceTag
type VkFence = Ptr VkFenceTag

data VkImageTag
type VkImage = Ptr VkImageTag

data VkImageViewTag
type VkImageView = Ptr VkImageViewTag

data VkInstanceTag
type VkInstance = Ptr VkInstanceTag

data VkPhysicalDeviceTag
type VkPhysicalDevice = Ptr VkPhysicalDeviceTag

data VkPipelineTag
type VkPipeline = Ptr VkPipelineTag

data VkPipelineCacheTag
type VkPipelineCache = Ptr VkPipelineCacheTag

data VkPipelineLayoutTag
type VkPipelineLayout = Ptr VkPipelineLayoutTag

data VkQueryPoolTag
type VkQueryPool = Ptr VkQueryPoolTag

data VkQueueTag
type VkQueue = Ptr VkQueueTag

data VkSamplerTag
type VkSampler = Ptr VkSamplerTag

data VkSemaphoreTag
type VkSemaphore = Ptr VkSemaphoreTag

data VkShaderModuleTag
type VkShaderModule = Ptr VkShaderModuleTag

data VkSurfaceKHRTag
type VkSurfaceKHR = Ptr VkSurfaceKHRTag

data VkSwapchainKHRTag
type VkSwapchainKHR = Ptr VkSwapchainKHRTag

type InstanceAddress = VkInstance -> String -> IO (FunPtr ())
type DeviceAddress = VkDevice -> String -> IO (FunPtr ())

withVulkan :: IO Bool -> (InstanceAddress -> IO a) -> IO a
withVulkan allowRelease use = bracket load close $ \library -> do
  proc <- withCString "vkGetInstanceProcAddr" (getProcAddress library)
  when (proc == nullFunPtr) (throwIO (VulkanFunctionUnavailable "vkGetInstanceProcAddr"))
  let find instanceHandle name = withCString name $ \encoded ->
        requireFunction name =<< instanceProcAddress (castFunPtr proc) instanceHandle encoded
  use find
  where
    load = withArray0 0 (map (fromIntegral . fromEnum) "vulkan-1.dll") $ \name -> do
      library <- loadLibrary name nullPtr loadLibrarySearchSystem32
      if library == nullPtr
        then getLastError >>= throwIO . VulkanLoaderUnavailable
        else pure library
    close library = do
      allowed <- allowRelease
      when allowed (freeLibrary library >> pure ())

deviceAddress :: InstanceFns -> DeviceAddress
deviceAddress fns device name = withCString name $ \encoded ->
  requireFunction name =<< vkGetDeviceProcAddr fns device encoded

requireFunction :: String -> FunPtr () -> IO (FunPtr ())
requireFunction name address
  | address == nullFunPtr = throwIO (VulkanFunctionUnavailable name)
  | otherwise = pure address

vkCheck :: String -> IO Int32 -> IO ()
vkCheck label action = do
  result <- action
  when (result /= VK_SUCCESS) (throwIO (VulkanCallFailed label result))

loadLibrarySearchSystem32 :: Word32
loadLibrarySearchSystem32 = 0x00000800

foreign import ccall safe "LoadLibraryExW"
  loadLibrary :: Ptr Word16 -> Ptr () -> Word32 -> IO (Ptr ())
foreign import ccall safe "FreeLibrary"
  freeLibrary :: Ptr () -> IO Int32
foreign import ccall unsafe "GetLastError"
  getLastError :: IO Word32
foreign import ccall unsafe "GetProcAddress"
  getProcAddress :: Ptr () -> CString -> IO (FunPtr ())
foreign import ccall safe "dynamic"
  instanceProcAddress
    :: FunPtr (VkInstance -> CString -> IO (FunPtr ())) -> VkInstance -> CString -> IO (FunPtr ())

-- Recording commands marked unsafe below just append to an externally
-- synchronized command buffer. Driver/WSI calls that can block or re-enter
-- the window procedure are safe. This renderer never registers Vulkan callbacks.
data GlobalFns = GlobalFns
  { vkEnumerateInstanceVersion :: Ptr Word32 -> IO Int32
  , vkEnumerateInstanceExtensionProperties :: CString -> Ptr Word32 -> Ptr () -> IO Int32
  , vkCreateInstance :: Ptr () -> Ptr () -> Ptr VkInstance -> IO Int32
  }

loadGlobalFns :: (String -> IO (FunPtr ())) -> IO GlobalFns
loadGlobalFns address = GlobalFns
  <$> (callEnumerateInstanceVersion . castFunPtr <$> address "vkEnumerateInstanceVersion")
  <*> (callEnumerateInstanceExtensionProperties . castFunPtr <$> address "vkEnumerateInstanceExtensionProperties")
  <*> (callCreateInstance . castFunPtr <$> address "vkCreateInstance")

foreign import ccall safe "dynamic"
  callEnumerateInstanceVersion :: FunPtr (Ptr Word32 -> IO Int32) -> Ptr Word32 -> IO Int32
foreign import ccall safe "dynamic"
  callEnumerateInstanceExtensionProperties
    :: FunPtr (CString -> Ptr Word32 -> Ptr () -> IO Int32) -> CString -> Ptr Word32 -> Ptr () -> IO Int32
foreign import ccall safe "dynamic"
  callCreateInstance :: FunPtr (Ptr () -> Ptr () -> Ptr VkInstance -> IO Int32) -> Ptr () -> Ptr () -> Ptr VkInstance -> IO Int32

data InstanceFns = InstanceFns
  { vkDestroyInstance :: VkInstance -> Ptr () -> IO ()
  , vkGetDeviceProcAddr :: VkDevice -> CString -> IO (FunPtr ())
  , vkEnumeratePhysicalDevices :: VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO Int32
  , vkGetPhysicalDeviceProperties :: VkPhysicalDevice -> Ptr () -> IO ()
  , vkGetPhysicalDeviceFeatures2 :: VkPhysicalDevice -> Ptr () -> IO ()
  , vkGetPhysicalDeviceQueueFamilyProperties :: VkPhysicalDevice -> Ptr Word32 -> Ptr () -> IO ()
  , vkGetPhysicalDeviceMemoryProperties :: VkPhysicalDevice -> Ptr () -> IO ()
  , vkGetPhysicalDeviceFormatProperties :: VkPhysicalDevice -> Word32 -> Ptr () -> IO ()
  , vkEnumerateDeviceExtensionProperties :: VkPhysicalDevice -> CString -> Ptr Word32 -> Ptr () -> IO Int32
  , vkCreateDevice :: VkPhysicalDevice -> Ptr () -> Ptr () -> Ptr VkDevice -> IO Int32
  , vkCreateWin32SurfaceKHR :: VkInstance -> Ptr () -> Ptr () -> Ptr VkSurfaceKHR -> IO Int32
  , vkDestroySurfaceKHR :: VkInstance -> VkSurfaceKHR -> Ptr () -> IO ()
  , vkGetPhysicalDeviceSurfaceSupportKHR :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> Ptr Word32 -> IO Int32
  , vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: VkPhysicalDevice -> VkSurfaceKHR -> Ptr () -> IO Int32
  , vkGetPhysicalDeviceSurfaceFormatsKHR :: VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr () -> IO Int32
  }

loadInstanceFns :: (String -> IO (FunPtr ())) -> IO InstanceFns
loadInstanceFns address = InstanceFns
  <$> (callDestroyInstance . castFunPtr <$> address "vkDestroyInstance")
  <*> (callGetDeviceProcAddr . castFunPtr <$> address "vkGetDeviceProcAddr")
  <*> (callEnumeratePhysicalDevices . castFunPtr <$> address "vkEnumeratePhysicalDevices")
  <*> (callGetPhysicalDeviceProperties . castFunPtr <$> address "vkGetPhysicalDeviceProperties")
  <*> (callGetPhysicalDeviceFeatures2 . castFunPtr <$> address "vkGetPhysicalDeviceFeatures2")
  <*> (callGetPhysicalDeviceQueueFamilyProperties . castFunPtr <$> address "vkGetPhysicalDeviceQueueFamilyProperties")
  <*> (callGetPhysicalDeviceMemoryProperties . castFunPtr <$> address "vkGetPhysicalDeviceMemoryProperties")
  <*> (callGetPhysicalDeviceFormatProperties . castFunPtr <$> address "vkGetPhysicalDeviceFormatProperties")
  <*> (callEnumerateDeviceExtensionProperties . castFunPtr <$> address "vkEnumerateDeviceExtensionProperties")
  <*> (callCreateDevice . castFunPtr <$> address "vkCreateDevice")
  <*> (callCreateWin32SurfaceKHR . castFunPtr <$> address "vkCreateWin32SurfaceKHR")
  <*> (callDestroySurfaceKHR . castFunPtr <$> address "vkDestroySurfaceKHR")
  <*> (callGetPhysicalDeviceSurfaceSupportKHR . castFunPtr <$> address "vkGetPhysicalDeviceSurfaceSupportKHR")
  <*> (callGetPhysicalDeviceSurfaceCapabilitiesKHR . castFunPtr <$> address "vkGetPhysicalDeviceSurfaceCapabilitiesKHR")
  <*> (callGetPhysicalDeviceSurfaceFormatsKHR . castFunPtr <$> address "vkGetPhysicalDeviceSurfaceFormatsKHR")

foreign import ccall safe "dynamic"
  callDestroyInstance :: FunPtr (VkInstance -> Ptr () -> IO ()) -> VkInstance -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetDeviceProcAddr :: FunPtr (VkDevice -> CString -> IO (FunPtr ())) -> VkDevice -> CString -> IO (FunPtr ())
foreign import ccall safe "dynamic"
  callEnumeratePhysicalDevices :: FunPtr (VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO Int32) -> VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO Int32
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceProperties :: FunPtr (VkPhysicalDevice -> Ptr () -> IO ()) -> VkPhysicalDevice -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceFeatures2 :: FunPtr (VkPhysicalDevice -> Ptr () -> IO ()) -> VkPhysicalDevice -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceQueueFamilyProperties :: FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr () -> IO ()) -> VkPhysicalDevice -> Ptr Word32 -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceMemoryProperties :: FunPtr (VkPhysicalDevice -> Ptr () -> IO ()) -> VkPhysicalDevice -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceFormatProperties :: FunPtr (VkPhysicalDevice -> Word32 -> Ptr () -> IO ()) -> VkPhysicalDevice -> Word32 -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callEnumerateDeviceExtensionProperties :: FunPtr (VkPhysicalDevice -> CString -> Ptr Word32 -> Ptr () -> IO Int32) -> VkPhysicalDevice -> CString -> Ptr Word32 -> Ptr () -> IO Int32
foreign import ccall safe "dynamic"
  callCreateDevice :: FunPtr (VkPhysicalDevice -> Ptr () -> Ptr () -> Ptr VkDevice -> IO Int32) -> VkPhysicalDevice -> Ptr () -> Ptr () -> Ptr VkDevice -> IO Int32
foreign import ccall safe "dynamic"
  callCreateWin32SurfaceKHR :: FunPtr (VkInstance -> Ptr () -> Ptr () -> Ptr VkSurfaceKHR -> IO Int32) -> VkInstance -> Ptr () -> Ptr () -> Ptr VkSurfaceKHR -> IO Int32
foreign import ccall safe "dynamic"
  callDestroySurfaceKHR :: FunPtr (VkInstance -> VkSurfaceKHR -> Ptr () -> IO ()) -> VkInstance -> VkSurfaceKHR -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceSurfaceSupportKHR :: FunPtr (VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> Ptr Word32 -> IO Int32) -> VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> Ptr Word32 -> IO Int32
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceSurfaceCapabilitiesKHR :: FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr () -> IO Int32) -> VkPhysicalDevice -> VkSurfaceKHR -> Ptr () -> IO Int32
foreign import ccall safe "dynamic"
  callGetPhysicalDeviceSurfaceFormatsKHR :: FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr () -> IO Int32) -> VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr () -> IO Int32

data DeviceFns = DeviceFns
  { vkDestroyDevice :: VkDevice -> Ptr () -> IO ()
  , vkGetDeviceQueue :: VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()
  , vkDeviceWaitIdle :: VkDevice -> IO Int32
  , vkCreateSwapchainKHR :: VkDevice -> Ptr () -> Ptr () -> Ptr VkSwapchainKHR -> IO Int32
  , vkDestroySwapchainKHR :: VkDevice -> VkSwapchainKHR -> Ptr () -> IO ()
  , vkGetSwapchainImagesKHR :: VkDevice -> VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO Int32
  , vkAcquireNextImageKHR :: VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO Int32
  , vkQueuePresentKHR :: VkQueue -> Ptr () -> IO Int32
  , vkQueueSubmit :: VkQueue -> Word32 -> Ptr () -> VkFence -> IO Int32
  , vkCreateSemaphore :: VkDevice -> Ptr () -> Ptr () -> Ptr VkSemaphore -> IO Int32
  , vkDestroySemaphore :: VkDevice -> VkSemaphore -> Ptr () -> IO ()
  , vkCreateFence :: VkDevice -> Ptr () -> Ptr () -> Ptr VkFence -> IO Int32
  , vkDestroyFence :: VkDevice -> VkFence -> Ptr () -> IO ()
  , vkWaitForFences :: VkDevice -> Word32 -> Ptr VkFence -> Word32 -> Word64 -> IO Int32
  , vkResetFences :: VkDevice -> Word32 -> Ptr VkFence -> IO Int32
  , vkCreateImageView :: VkDevice -> Ptr () -> Ptr () -> Ptr VkImageView -> IO Int32
  , vkDestroyImageView :: VkDevice -> VkImageView -> Ptr () -> IO ()
  , vkCreateCommandPool :: VkDevice -> Ptr () -> Ptr () -> Ptr VkCommandPool -> IO Int32
  , vkDestroyCommandPool :: VkDevice -> VkCommandPool -> Ptr () -> IO ()
  , vkAllocateCommandBuffers :: VkDevice -> Ptr () -> Ptr VkCommandBuffer -> IO Int32
  , vkResetCommandBuffer :: VkCommandBuffer -> Word32 -> IO Int32
  , vkBeginCommandBuffer :: VkCommandBuffer -> Ptr () -> IO Int32
  , vkEndCommandBuffer :: VkCommandBuffer -> IO Int32
  , vkCmdPipelineBarrier :: VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> Ptr () -> Word32 -> Ptr () -> Word32 -> Ptr () -> IO ()
  , vkCmdBeginRendering :: VkCommandBuffer -> Ptr () -> IO ()
  , vkCmdEndRendering :: VkCommandBuffer -> IO ()
  , vkCmdCopyImageToBuffer :: VkCommandBuffer -> VkImage -> Word32 -> VkBuffer -> Word32 -> Ptr () -> IO ()
  , vkCmdCopyBufferToImage :: VkCommandBuffer -> VkBuffer -> VkImage -> Word32 -> Word32 -> Ptr () -> IO ()
  , vkCmdCopyBuffer :: VkCommandBuffer -> VkBuffer -> VkBuffer -> Word32 -> Ptr () -> IO ()
  , vkCreateImage :: VkDevice -> Ptr () -> Ptr () -> Ptr VkImage -> IO Int32
  , vkDestroyImage :: VkDevice -> VkImage -> Ptr () -> IO ()
  , vkGetImageMemoryRequirements :: VkDevice -> VkImage -> Ptr () -> IO ()
  , vkBindImageMemory :: VkDevice -> VkImage -> VkDeviceMemory -> Word64 -> IO Int32
  , vkCreateBuffer :: VkDevice -> Ptr () -> Ptr () -> Ptr VkBuffer -> IO Int32
  , vkDestroyBuffer :: VkDevice -> VkBuffer -> Ptr () -> IO ()
  , vkGetBufferMemoryRequirements :: VkDevice -> VkBuffer -> Ptr () -> IO ()
  , vkBindBufferMemory :: VkDevice -> VkBuffer -> VkDeviceMemory -> Word64 -> IO Int32
  , vkAllocateMemory :: VkDevice -> Ptr () -> Ptr () -> Ptr VkDeviceMemory -> IO Int32
  , vkFreeMemory :: VkDevice -> VkDeviceMemory -> Ptr () -> IO ()
  , vkMapMemory :: VkDevice -> VkDeviceMemory -> Word64 -> Word64 -> Word32 -> Ptr (Ptr ()) -> IO Int32
  , vkUnmapMemory :: VkDevice -> VkDeviceMemory -> IO ()
  , vkCreateShaderModule :: VkDevice -> Ptr () -> Ptr () -> Ptr VkShaderModule -> IO Int32
  , vkDestroyShaderModule :: VkDevice -> VkShaderModule -> Ptr () -> IO ()
  , vkCreatePipelineLayout :: VkDevice -> Ptr () -> Ptr () -> Ptr VkPipelineLayout -> IO Int32
  , vkDestroyPipelineLayout :: VkDevice -> VkPipelineLayout -> Ptr () -> IO ()
  , vkCreateGraphicsPipelines :: VkDevice -> VkPipelineCache -> Word32 -> Ptr () -> Ptr () -> Ptr VkPipeline -> IO Int32
  , vkDestroyPipeline :: VkDevice -> VkPipeline -> Ptr () -> IO ()
  , vkCmdBindPipeline :: VkCommandBuffer -> Word32 -> VkPipeline -> IO ()
  , vkCmdSetViewport :: VkCommandBuffer -> Word32 -> Word32 -> Ptr () -> IO ()
  , vkCmdSetScissor :: VkCommandBuffer -> Word32 -> Word32 -> Ptr () -> IO ()
  , vkCmdBindVertexBuffers :: VkCommandBuffer -> Word32 -> Word32 -> Ptr VkBuffer -> Ptr Word64 -> IO ()
  , vkCmdBindIndexBuffer :: VkCommandBuffer -> VkBuffer -> Word64 -> Word32 -> IO ()
  , vkCmdDraw :: VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
  , vkCmdDrawIndexed :: VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
  , vkCmdPushConstants :: VkCommandBuffer -> VkPipelineLayout -> Word32 -> Word32 -> Word32 -> Ptr () -> IO ()
  , vkCreateSampler :: VkDevice -> Ptr () -> Ptr () -> Ptr VkSampler -> IO Int32
  , vkDestroySampler :: VkDevice -> VkSampler -> Ptr () -> IO ()
  , vkCreateDescriptorSetLayout :: VkDevice -> Ptr () -> Ptr () -> Ptr VkDescriptorSetLayout -> IO Int32
  , vkDestroyDescriptorSetLayout :: VkDevice -> VkDescriptorSetLayout -> Ptr () -> IO ()
  , vkCreateDescriptorPool :: VkDevice -> Ptr () -> Ptr () -> Ptr VkDescriptorPool -> IO Int32
  , vkDestroyDescriptorPool :: VkDevice -> VkDescriptorPool -> Ptr () -> IO ()
  , vkAllocateDescriptorSets :: VkDevice -> Ptr () -> Ptr VkDescriptorSet -> IO Int32
  , vkFreeDescriptorSets :: VkDevice -> VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO Int32
  , vkUpdateDescriptorSets :: VkDevice -> Word32 -> Ptr () -> Word32 -> Ptr () -> IO ()
  , vkCmdBindDescriptorSets :: VkCommandBuffer -> Word32 -> VkPipelineLayout -> Word32 -> Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()
  , vkCreateQueryPool :: VkDevice -> Ptr () -> Ptr () -> Ptr VkQueryPool -> IO Int32
  , vkDestroyQueryPool :: VkDevice -> VkQueryPool -> Ptr () -> IO ()
  , vkCmdResetQueryPool :: VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ()
  , vkCmdWriteTimestamp :: VkCommandBuffer -> Word32 -> VkQueryPool -> Word32 -> IO ()
  , vkGetQueryPoolResults :: VkDevice -> VkQueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> Word64 -> Word32 -> IO Int32
  }

loadDeviceFns :: (String -> IO (FunPtr ())) -> IO DeviceFns
loadDeviceFns address = DeviceFns
  <$> (callDestroyDevice . castFunPtr <$> address "vkDestroyDevice")
  <*> (callGetDeviceQueue . castFunPtr <$> address "vkGetDeviceQueue")
  <*> (callDeviceWaitIdle . castFunPtr <$> address "vkDeviceWaitIdle")
  <*> (callCreateSwapchainKHR . castFunPtr <$> address "vkCreateSwapchainKHR")
  <*> (callDestroySwapchainKHR . castFunPtr <$> address "vkDestroySwapchainKHR")
  <*> (callGetSwapchainImagesKHR . castFunPtr <$> address "vkGetSwapchainImagesKHR")
  <*> (callAcquireNextImageKHR . castFunPtr <$> address "vkAcquireNextImageKHR")
  <*> (callQueuePresentKHR . castFunPtr <$> address "vkQueuePresentKHR")
  <*> (callQueueSubmit . castFunPtr <$> address "vkQueueSubmit")
  <*> (callCreateSemaphore . castFunPtr <$> address "vkCreateSemaphore")
  <*> (callDestroySemaphore . castFunPtr <$> address "vkDestroySemaphore")
  <*> (callCreateFence . castFunPtr <$> address "vkCreateFence")
  <*> (callDestroyFence . castFunPtr <$> address "vkDestroyFence")
  <*> (callWaitForFences . castFunPtr <$> address "vkWaitForFences")
  <*> (callResetFences . castFunPtr <$> address "vkResetFences")
  <*> (callCreateImageView . castFunPtr <$> address "vkCreateImageView")
  <*> (callDestroyImageView . castFunPtr <$> address "vkDestroyImageView")
  <*> (callCreateCommandPool . castFunPtr <$> address "vkCreateCommandPool")
  <*> (callDestroyCommandPool . castFunPtr <$> address "vkDestroyCommandPool")
  <*> (callAllocateCommandBuffers . castFunPtr <$> address "vkAllocateCommandBuffers")
  <*> (callResetCommandBuffer . castFunPtr <$> address "vkResetCommandBuffer")
  <*> (callBeginCommandBuffer . castFunPtr <$> address "vkBeginCommandBuffer")
  <*> (callEndCommandBuffer . castFunPtr <$> address "vkEndCommandBuffer")
  <*> (callCmdPipelineBarrier . castFunPtr <$> address "vkCmdPipelineBarrier")
  <*> (callCmdBeginRendering . castFunPtr <$> address "vkCmdBeginRendering")
  <*> (callCmdEndRendering . castFunPtr <$> address "vkCmdEndRendering")
  <*> (callCmdCopyImageToBuffer . castFunPtr <$> address "vkCmdCopyImageToBuffer")
  <*> (callCmdCopyBufferToImage . castFunPtr <$> address "vkCmdCopyBufferToImage")
  <*> (callCmdCopyBuffer . castFunPtr <$> address "vkCmdCopyBuffer")
  <*> (callCreateImage . castFunPtr <$> address "vkCreateImage")
  <*> (callDestroyImage . castFunPtr <$> address "vkDestroyImage")
  <*> (callGetImageMemoryRequirements . castFunPtr <$> address "vkGetImageMemoryRequirements")
  <*> (callBindImageMemory . castFunPtr <$> address "vkBindImageMemory")
  <*> (callCreateBuffer . castFunPtr <$> address "vkCreateBuffer")
  <*> (callDestroyBuffer . castFunPtr <$> address "vkDestroyBuffer")
  <*> (callGetBufferMemoryRequirements . castFunPtr <$> address "vkGetBufferMemoryRequirements")
  <*> (callBindBufferMemory . castFunPtr <$> address "vkBindBufferMemory")
  <*> (callAllocateMemory . castFunPtr <$> address "vkAllocateMemory")
  <*> (callFreeMemory . castFunPtr <$> address "vkFreeMemory")
  <*> (callMapMemory . castFunPtr <$> address "vkMapMemory")
  <*> (callUnmapMemory . castFunPtr <$> address "vkUnmapMemory")
  <*> (callCreateShaderModule . castFunPtr <$> address "vkCreateShaderModule")
  <*> (callDestroyShaderModule . castFunPtr <$> address "vkDestroyShaderModule")
  <*> (callCreatePipelineLayout . castFunPtr <$> address "vkCreatePipelineLayout")
  <*> (callDestroyPipelineLayout . castFunPtr <$> address "vkDestroyPipelineLayout")
  <*> (callCreateGraphicsPipelines . castFunPtr <$> address "vkCreateGraphicsPipelines")
  <*> (callDestroyPipeline . castFunPtr <$> address "vkDestroyPipeline")
  <*> (callCmdBindPipeline . castFunPtr <$> address "vkCmdBindPipeline")
  <*> (callCmdSetViewport . castFunPtr <$> address "vkCmdSetViewport")
  <*> (callCmdSetScissor . castFunPtr <$> address "vkCmdSetScissor")
  <*> (callCmdBindVertexBuffers . castFunPtr <$> address "vkCmdBindVertexBuffers")
  <*> (callCmdBindIndexBuffer . castFunPtr <$> address "vkCmdBindIndexBuffer")
  <*> (callCmdDraw . castFunPtr <$> address "vkCmdDraw")
  <*> (callCmdDrawIndexed . castFunPtr <$> address "vkCmdDrawIndexed")
  <*> (callCmdPushConstants . castFunPtr <$> address "vkCmdPushConstants")
  <*> (callCreateSampler . castFunPtr <$> address "vkCreateSampler")
  <*> (callDestroySampler . castFunPtr <$> address "vkDestroySampler")
  <*> (callCreateDescriptorSetLayout . castFunPtr <$> address "vkCreateDescriptorSetLayout")
  <*> (callDestroyDescriptorSetLayout . castFunPtr <$> address "vkDestroyDescriptorSetLayout")
  <*> (callCreateDescriptorPool . castFunPtr <$> address "vkCreateDescriptorPool")
  <*> (callDestroyDescriptorPool . castFunPtr <$> address "vkDestroyDescriptorPool")
  <*> (callAllocateDescriptorSets . castFunPtr <$> address "vkAllocateDescriptorSets")
  <*> (callFreeDescriptorSets . castFunPtr <$> address "vkFreeDescriptorSets")
  <*> (callUpdateDescriptorSets . castFunPtr <$> address "vkUpdateDescriptorSets")
  <*> (callCmdBindDescriptorSets . castFunPtr <$> address "vkCmdBindDescriptorSets")
  <*> (callCreateQueryPool . castFunPtr <$> address "vkCreateQueryPool")
  <*> (callDestroyQueryPool . castFunPtr <$> address "vkDestroyQueryPool")
  <*> (callCmdResetQueryPool . castFunPtr <$> address "vkCmdResetQueryPool")
  <*> (callCmdWriteTimestamp . castFunPtr <$> address "vkCmdWriteTimestamp")
  <*> (callGetQueryPoolResults . castFunPtr <$> address "vkGetQueryPoolResults")

foreign import ccall safe "dynamic"
  callDestroyDevice :: FunPtr (VkDevice -> Ptr () -> IO ()) -> VkDevice -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetDeviceQueue :: FunPtr (VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()) -> VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()
foreign import ccall safe "dynamic"
  callDeviceWaitIdle :: FunPtr (VkDevice -> IO Int32) -> VkDevice -> IO Int32
foreign import ccall safe "dynamic"
  callCreateSwapchainKHR :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkSwapchainKHR -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkSwapchainKHR -> IO Int32
foreign import ccall safe "dynamic"
  callDestroySwapchainKHR :: FunPtr (VkDevice -> VkSwapchainKHR -> Ptr () -> IO ()) -> VkDevice -> VkSwapchainKHR -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetSwapchainImagesKHR :: FunPtr (VkDevice -> VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO Int32) -> VkDevice -> VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO Int32
foreign import ccall safe "dynamic"
  callAcquireNextImageKHR :: FunPtr (VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO Int32) -> VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO Int32
foreign import ccall safe "dynamic"
  callQueuePresentKHR :: FunPtr (VkQueue -> Ptr () -> IO Int32) -> VkQueue -> Ptr () -> IO Int32
foreign import ccall safe "dynamic"
  callQueueSubmit :: FunPtr (VkQueue -> Word32 -> Ptr () -> VkFence -> IO Int32) -> VkQueue -> Word32 -> Ptr () -> VkFence -> IO Int32
foreign import ccall safe "dynamic"
  callCreateSemaphore :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkSemaphore -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkSemaphore -> IO Int32
foreign import ccall safe "dynamic"
  callDestroySemaphore :: FunPtr (VkDevice -> VkSemaphore -> Ptr () -> IO ()) -> VkDevice -> VkSemaphore -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreateFence :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkFence -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkFence -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyFence :: FunPtr (VkDevice -> VkFence -> Ptr () -> IO ()) -> VkDevice -> VkFence -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callWaitForFences :: FunPtr (VkDevice -> Word32 -> Ptr VkFence -> Word32 -> Word64 -> IO Int32) -> VkDevice -> Word32 -> Ptr VkFence -> Word32 -> Word64 -> IO Int32
foreign import ccall safe "dynamic"
  callResetFences :: FunPtr (VkDevice -> Word32 -> Ptr VkFence -> IO Int32) -> VkDevice -> Word32 -> Ptr VkFence -> IO Int32
foreign import ccall safe "dynamic"
  callCreateImageView :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkImageView -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkImageView -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyImageView :: FunPtr (VkDevice -> VkImageView -> Ptr () -> IO ()) -> VkDevice -> VkImageView -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreateCommandPool :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkCommandPool -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkCommandPool -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyCommandPool :: FunPtr (VkDevice -> VkCommandPool -> Ptr () -> IO ()) -> VkDevice -> VkCommandPool -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callAllocateCommandBuffers :: FunPtr (VkDevice -> Ptr () -> Ptr VkCommandBuffer -> IO Int32) -> VkDevice -> Ptr () -> Ptr VkCommandBuffer -> IO Int32
foreign import ccall safe "dynamic"
  callResetCommandBuffer :: FunPtr (VkCommandBuffer -> Word32 -> IO Int32) -> VkCommandBuffer -> Word32 -> IO Int32
foreign import ccall safe "dynamic"
  callBeginCommandBuffer :: FunPtr (VkCommandBuffer -> Ptr () -> IO Int32) -> VkCommandBuffer -> Ptr () -> IO Int32
foreign import ccall safe "dynamic"
  callEndCommandBuffer :: FunPtr (VkCommandBuffer -> IO Int32) -> VkCommandBuffer -> IO Int32
foreign import ccall unsafe "dynamic"
  callCmdPipelineBarrier :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> Ptr () -> Word32 -> Ptr () -> Word32 -> Ptr () -> IO ()) -> VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> Ptr () -> Word32 -> Ptr () -> Word32 -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdBeginRendering :: FunPtr (VkCommandBuffer -> Ptr () -> IO ()) -> VkCommandBuffer -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdEndRendering :: FunPtr (VkCommandBuffer -> IO ()) -> VkCommandBuffer -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdCopyImageToBuffer :: FunPtr (VkCommandBuffer -> VkImage -> Word32 -> VkBuffer -> Word32 -> Ptr () -> IO ()) -> VkCommandBuffer -> VkImage -> Word32 -> VkBuffer -> Word32 -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdCopyBufferToImage :: FunPtr (VkCommandBuffer -> VkBuffer -> VkImage -> Word32 -> Word32 -> Ptr () -> IO ()) -> VkCommandBuffer -> VkBuffer -> VkImage -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdCopyBuffer :: FunPtr (VkCommandBuffer -> VkBuffer -> VkBuffer -> Word32 -> Ptr () -> IO ()) -> VkCommandBuffer -> VkBuffer -> VkBuffer -> Word32 -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreateImage :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkImage -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkImage -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyImage :: FunPtr (VkDevice -> VkImage -> Ptr () -> IO ()) -> VkDevice -> VkImage -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetImageMemoryRequirements :: FunPtr (VkDevice -> VkImage -> Ptr () -> IO ()) -> VkDevice -> VkImage -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callBindImageMemory :: FunPtr (VkDevice -> VkImage -> VkDeviceMemory -> Word64 -> IO Int32) -> VkDevice -> VkImage -> VkDeviceMemory -> Word64 -> IO Int32
foreign import ccall safe "dynamic"
  callCreateBuffer :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkBuffer -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkBuffer -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyBuffer :: FunPtr (VkDevice -> VkBuffer -> Ptr () -> IO ()) -> VkDevice -> VkBuffer -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callGetBufferMemoryRequirements :: FunPtr (VkDevice -> VkBuffer -> Ptr () -> IO ()) -> VkDevice -> VkBuffer -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callBindBufferMemory :: FunPtr (VkDevice -> VkBuffer -> VkDeviceMemory -> Word64 -> IO Int32) -> VkDevice -> VkBuffer -> VkDeviceMemory -> Word64 -> IO Int32
foreign import ccall safe "dynamic"
  callAllocateMemory :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkDeviceMemory -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkDeviceMemory -> IO Int32
foreign import ccall safe "dynamic"
  callFreeMemory :: FunPtr (VkDevice -> VkDeviceMemory -> Ptr () -> IO ()) -> VkDevice -> VkDeviceMemory -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callMapMemory :: FunPtr (VkDevice -> VkDeviceMemory -> Word64 -> Word64 -> Word32 -> Ptr (Ptr ()) -> IO Int32) -> VkDevice -> VkDeviceMemory -> Word64 -> Word64 -> Word32 -> Ptr (Ptr ()) -> IO Int32
foreign import ccall safe "dynamic"
  callUnmapMemory :: FunPtr (VkDevice -> VkDeviceMemory -> IO ()) -> VkDevice -> VkDeviceMemory -> IO ()
foreign import ccall safe "dynamic"
  callCreateShaderModule :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkShaderModule -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkShaderModule -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyShaderModule :: FunPtr (VkDevice -> VkShaderModule -> Ptr () -> IO ()) -> VkDevice -> VkShaderModule -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreatePipelineLayout :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkPipelineLayout -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkPipelineLayout -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyPipelineLayout :: FunPtr (VkDevice -> VkPipelineLayout -> Ptr () -> IO ()) -> VkDevice -> VkPipelineLayout -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreateGraphicsPipelines :: FunPtr (VkDevice -> VkPipelineCache -> Word32 -> Ptr () -> Ptr () -> Ptr VkPipeline -> IO Int32) -> VkDevice -> VkPipelineCache -> Word32 -> Ptr () -> Ptr () -> Ptr VkPipeline -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyPipeline :: FunPtr (VkDevice -> VkPipeline -> Ptr () -> IO ()) -> VkDevice -> VkPipeline -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdBindPipeline :: FunPtr (VkCommandBuffer -> Word32 -> VkPipeline -> IO ()) -> VkCommandBuffer -> Word32 -> VkPipeline -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdSetViewport :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr () -> IO ()) -> VkCommandBuffer -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdSetScissor :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr () -> IO ()) -> VkCommandBuffer -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdBindVertexBuffers :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkBuffer -> Ptr Word64 -> IO ()) -> VkCommandBuffer -> Word32 -> Word32 -> Ptr VkBuffer -> Ptr Word64 -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdBindIndexBuffer :: FunPtr (VkCommandBuffer -> VkBuffer -> Word64 -> Word32 -> IO ()) -> VkCommandBuffer -> VkBuffer -> Word64 -> Word32 -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdDraw :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdDrawIndexed :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()) -> VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdPushConstants :: FunPtr (VkCommandBuffer -> VkPipelineLayout -> Word32 -> Word32 -> Word32 -> Ptr () -> IO ()) -> VkCommandBuffer -> VkPipelineLayout -> Word32 -> Word32 -> Word32 -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreateSampler :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkSampler -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkSampler -> IO Int32
foreign import ccall safe "dynamic"
  callDestroySampler :: FunPtr (VkDevice -> VkSampler -> Ptr () -> IO ()) -> VkDevice -> VkSampler -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreateDescriptorSetLayout :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkDescriptorSetLayout -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkDescriptorSetLayout -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyDescriptorSetLayout :: FunPtr (VkDevice -> VkDescriptorSetLayout -> Ptr () -> IO ()) -> VkDevice -> VkDescriptorSetLayout -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callCreateDescriptorPool :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkDescriptorPool -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkDescriptorPool -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyDescriptorPool :: FunPtr (VkDevice -> VkDescriptorPool -> Ptr () -> IO ()) -> VkDevice -> VkDescriptorPool -> Ptr () -> IO ()
foreign import ccall safe "dynamic"
  callAllocateDescriptorSets :: FunPtr (VkDevice -> Ptr () -> Ptr VkDescriptorSet -> IO Int32) -> VkDevice -> Ptr () -> Ptr VkDescriptorSet -> IO Int32
foreign import ccall safe "dynamic"
  callFreeDescriptorSets :: FunPtr (VkDevice -> VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO Int32) -> VkDevice -> VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO Int32
foreign import ccall safe "dynamic"
  callUpdateDescriptorSets :: FunPtr (VkDevice -> Word32 -> Ptr () -> Word32 -> Ptr () -> IO ()) -> VkDevice -> Word32 -> Ptr () -> Word32 -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdBindDescriptorSets :: FunPtr (VkCommandBuffer -> Word32 -> VkPipelineLayout -> Word32 -> Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()) -> VkCommandBuffer -> Word32 -> VkPipelineLayout -> Word32 -> Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()
foreign import ccall safe "dynamic"
  callCreateQueryPool :: FunPtr (VkDevice -> Ptr () -> Ptr () -> Ptr VkQueryPool -> IO Int32) -> VkDevice -> Ptr () -> Ptr () -> Ptr VkQueryPool -> IO Int32
foreign import ccall safe "dynamic"
  callDestroyQueryPool :: FunPtr (VkDevice -> VkQueryPool -> Ptr () -> IO ()) -> VkDevice -> VkQueryPool -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdResetQueryPool :: FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ()) -> VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ()
foreign import ccall unsafe "dynamic"
  callCmdWriteTimestamp :: FunPtr (VkCommandBuffer -> Word32 -> VkQueryPool -> Word32 -> IO ()) -> VkCommandBuffer -> Word32 -> VkQueryPool -> Word32 -> IO ()
foreign import ccall safe "dynamic"
  callGetQueryPoolResults :: FunPtr (VkDevice -> VkQueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> Word64 -> Word32 -> IO Int32) -> VkDevice -> VkQueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> Word64 -> Word32 -> IO Int32

-- Vulkan enumerants used by the renderer.
pattern VK_SUCCESS :: Int32
pattern VK_SUCCESS = 0

pattern VK_NOT_READY :: Int32
pattern VK_NOT_READY = 1

pattern VK_TIMEOUT :: Int32
pattern VK_TIMEOUT = 2

pattern VK_INCOMPLETE :: Int32
pattern VK_INCOMPLETE = 5

pattern VK_SUBOPTIMAL_KHR :: Int32
pattern VK_SUBOPTIMAL_KHR = 1000001003

pattern VK_ERROR_OUT_OF_DATE_KHR :: Int32
pattern VK_ERROR_OUT_OF_DATE_KHR = -1000001004

pattern VK_ERROR_DEVICE_LOST :: Int32
pattern VK_ERROR_DEVICE_LOST = -4

pattern VK_ERROR_SURFACE_LOST_KHR :: Int32
pattern VK_ERROR_SURFACE_LOST_KHR = -1000000000

pattern VK_STRUCTURE_TYPE_APPLICATION_INFO :: Word32
pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = 0

pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = 1

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = 2

pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = 3

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 :: Word32
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 = 1000059000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES :: Word32
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES = 1000044003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR :: Word32
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR = 1000275000

pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR :: Word32
pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR = 1000009000

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR :: Word32
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR = 1000001000

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR :: Word32
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR = 1000275001

pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR :: Word32
pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR = 1000001001

pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = 9

pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = 8

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = 15

pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = 39

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = 40

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO :: Word32
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = 42

pattern VK_STRUCTURE_TYPE_SUBMIT_INFO :: Word32
pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = 4

pattern VK_STRUCTURE_TYPE_RENDERING_INFO :: Word32
pattern VK_STRUCTURE_TYPE_RENDERING_INFO = 1000044000

pattern VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO :: Word32
pattern VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO = 1000044001

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER :: Word32
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = 45

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER :: Word32
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = 44

pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER :: Word32
pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = 46

pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = 12

pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = 14

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = 5

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = 16

pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = 18

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = 19

pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = 20

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = 22

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = 23

pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = 24

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = 26

pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = 25

pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = 27

pattern VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO = 1000044002

pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = 30

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = 28

pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = 31

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = 32

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = 33

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = 34

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET :: Word32
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = 35

pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO :: Word32
pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = 11

pattern VK_QUEUE_GRAPHICS_BIT :: Word32
pattern VK_QUEUE_GRAPHICS_BIT = 0x00000001

pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR :: Word32
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = 0x00000001

pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR :: Word32
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = 0x00000001

pattern VK_PRESENT_MODE_FIFO_KHR :: Word32
pattern VK_PRESENT_MODE_FIFO_KHR = 2

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR :: Word32
pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = 0

pattern VK_FORMAT_UNDEFINED :: Word32
pattern VK_FORMAT_UNDEFINED = 0

pattern VK_FORMAT_B8G8R8A8_UNORM :: Word32
pattern VK_FORMAT_B8G8R8A8_UNORM = 44

pattern VK_FORMAT_R8G8B8A8_UNORM :: Word32
pattern VK_FORMAT_R8G8B8A8_UNORM = 37

pattern VK_FORMAT_B8G8R8A8_SRGB :: Word32
pattern VK_FORMAT_B8G8R8A8_SRGB = 50

pattern VK_FORMAT_R8G8B8A8_SRGB :: Word32
pattern VK_FORMAT_R8G8B8A8_SRGB = 43

pattern VK_FORMAT_D32_SFLOAT :: Word32
pattern VK_FORMAT_D32_SFLOAT = 126

pattern VK_FORMAT_R16G16B16A16_SFLOAT :: Word32
pattern VK_FORMAT_R16G16B16A16_SFLOAT = 97

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT :: Word32
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = 0x00000001

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT :: Word32
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = 0x00000080

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT :: Word32
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = 0x00000100

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT :: Word32
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = 0x00001000

pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT :: Word32
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT = 0x00008000

pattern VK_FORMAT_R32_SFLOAT :: Word32
pattern VK_FORMAT_R32_SFLOAT = 100

pattern VK_FORMAT_R32G32B32_SFLOAT :: Word32
pattern VK_FORMAT_R32G32B32_SFLOAT = 106

pattern VK_FORMAT_R32G32_SFLOAT :: Word32
pattern VK_FORMAT_R32G32_SFLOAT = 103

pattern VK_FORMAT_R32G32B32A32_SFLOAT :: Word32
pattern VK_FORMAT_R32G32B32A32_SFLOAT = 109

pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT :: Word32
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = 0x00000200

pattern VK_SHARING_MODE_EXCLUSIVE :: Word32
pattern VK_SHARING_MODE_EXCLUSIVE = 0

pattern VK_IMAGE_TYPE_2D :: Word32
pattern VK_IMAGE_TYPE_2D = 1

pattern VK_IMAGE_VIEW_TYPE_2D :: Word32
pattern VK_IMAGE_VIEW_TYPE_2D = 1

pattern VK_IMAGE_TILING_OPTIMAL :: Word32
pattern VK_IMAGE_TILING_OPTIMAL = 0

pattern VK_SAMPLE_COUNT_1_BIT :: Word32
pattern VK_SAMPLE_COUNT_1_BIT = 0x00000001

pattern VK_COMPONENT_SWIZZLE_IDENTITY :: Word32
pattern VK_COMPONENT_SWIZZLE_IDENTITY = 0

pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: Word32
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = 0x00000010

pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: Word32
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = 0x00000001

pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: Word32
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = 0x00000002

pattern VK_IMAGE_USAGE_SAMPLED_BIT :: Word32
pattern VK_IMAGE_USAGE_SAMPLED_BIT = 0x00000004

pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: Word32
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = 0x00000020

pattern VK_IMAGE_ASPECT_COLOR_BIT :: Word32
pattern VK_IMAGE_ASPECT_COLOR_BIT = 0x00000001

pattern VK_IMAGE_ASPECT_DEPTH_BIT :: Word32
pattern VK_IMAGE_ASPECT_DEPTH_BIT = 0x00000002

pattern VK_IMAGE_LAYOUT_UNDEFINED :: Word32
pattern VK_IMAGE_LAYOUT_UNDEFINED = 0

pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: Word32
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = 2

pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL :: Word32
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = 3

pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR :: Word32
pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR = 1000001002

pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: Word32
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = 6

pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: Word32
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = 7

pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: Word32
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = 5

pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: Word32
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = 0x00000100

pattern VK_ACCESS_TRANSFER_READ_BIT :: Word32
pattern VK_ACCESS_TRANSFER_READ_BIT = 0x00000800

pattern VK_ACCESS_TRANSFER_WRITE_BIT :: Word32
pattern VK_ACCESS_TRANSFER_WRITE_BIT = 0x00001000

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT :: Word32
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = 0x00000200

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT :: Word32
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = 0x00000400

pattern VK_ACCESS_SHADER_READ_BIT :: Word32
pattern VK_ACCESS_SHADER_READ_BIT = 0x00000020

pattern VK_ACCESS_HOST_READ_BIT :: Word32
pattern VK_ACCESS_HOST_READ_BIT = 0x00002000

pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: Word32
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = 0x00000004

pattern VK_ACCESS_INDEX_READ_BIT :: Word32
pattern VK_ACCESS_INDEX_READ_BIT = 0x00000002

pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT :: Word32
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = 0x00000001

pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT :: Word32
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = 0x00000400

pattern VK_PIPELINE_STAGE_TRANSFER_BIT :: Word32
pattern VK_PIPELINE_STAGE_TRANSFER_BIT = 0x00001000

pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT :: Word32
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = 0x00002000

pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT :: Word32
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = 0x00000100

pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT :: Word32
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = 0x00000200

pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT :: Word32
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = 0x00000080

pattern VK_PIPELINE_STAGE_HOST_BIT :: Word32
pattern VK_PIPELINE_STAGE_HOST_BIT = 0x00004000

pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT :: Word32
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = 0x00000004

pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT :: Word32
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = 0x00000008

pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT :: Word32
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = 0x00000002

pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: Word32
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = 0x00000001

pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: Word32
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = 0x00000080

pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT :: Word32
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = 0x00000010

pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: Word32
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = 0x00000040

pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT :: Word32
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = 0x00000002

pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT :: Word32
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = 0x00000004

pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: Word32
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = 0x00000001

pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT :: Word32
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = 0x00000002

pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY :: Word32
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = 0

pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT :: Word32
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = 0x00000001

pattern VK_FENCE_CREATE_SIGNALED_BIT :: Word32
pattern VK_FENCE_CREATE_SIGNALED_BIT = 0x00000001

pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: Word32
pattern VK_ATTACHMENT_LOAD_OP_CLEAR = 1

pattern VK_ATTACHMENT_LOAD_OP_LOAD :: Word32
pattern VK_ATTACHMENT_LOAD_OP_LOAD = 0

pattern VK_ATTACHMENT_STORE_OP_STORE :: Word32
pattern VK_ATTACHMENT_STORE_OP_STORE = 0

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: Word32
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = 1

pattern VK_RESOLVE_MODE_NONE :: Word32
pattern VK_RESOLVE_MODE_NONE = 0

pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: Word32
pattern VK_PIPELINE_BIND_POINT_GRAPHICS = 0

pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST :: Word32
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = 3

pattern VK_POLYGON_MODE_FILL :: Word32
pattern VK_POLYGON_MODE_FILL = 0

pattern VK_CULL_MODE_NONE :: Word32
pattern VK_CULL_MODE_NONE = 0

pattern VK_FRONT_FACE_COUNTER_CLOCKWISE :: Word32
pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = 0

pattern VK_VERTEX_INPUT_RATE_VERTEX :: Word32
pattern VK_VERTEX_INPUT_RATE_VERTEX = 0

pattern VK_VERTEX_INPUT_RATE_INSTANCE :: Word32
pattern VK_VERTEX_INPUT_RATE_INSTANCE = 1

pattern VK_INDEX_TYPE_UINT32 :: Word32
pattern VK_INDEX_TYPE_UINT32 = 1

pattern VK_COMPARE_OP_LESS :: Word32
pattern VK_COMPARE_OP_LESS = 1

pattern VK_COMPARE_OP_ALWAYS :: Word32
pattern VK_COMPARE_OP_ALWAYS = 7

pattern VK_DYNAMIC_STATE_VIEWPORT :: Word32
pattern VK_DYNAMIC_STATE_VIEWPORT = 0

pattern VK_DYNAMIC_STATE_SCISSOR :: Word32
pattern VK_DYNAMIC_STATE_SCISSOR = 1

pattern VK_COLOR_COMPONENT_R_BIT :: Word32
pattern VK_COLOR_COMPONENT_R_BIT = 0x00000001

pattern VK_COLOR_COMPONENT_G_BIT :: Word32
pattern VK_COLOR_COMPONENT_G_BIT = 0x00000002

pattern VK_COLOR_COMPONENT_B_BIT :: Word32
pattern VK_COLOR_COMPONENT_B_BIT = 0x00000004

pattern VK_COLOR_COMPONENT_A_BIT :: Word32
pattern VK_COLOR_COMPONENT_A_BIT = 0x00000008

pattern VK_BLEND_FACTOR_SRC_ALPHA :: Word32
pattern VK_BLEND_FACTOR_SRC_ALPHA = 6

pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA :: Word32
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = 7

pattern VK_BLEND_FACTOR_ONE :: Word32
pattern VK_BLEND_FACTOR_ONE = 1

pattern VK_BLEND_OP_ADD :: Word32
pattern VK_BLEND_OP_ADD = 0

pattern VK_SHADER_STAGE_VERTEX_BIT :: Word32
pattern VK_SHADER_STAGE_VERTEX_BIT = 0x00000001

pattern VK_SHADER_STAGE_FRAGMENT_BIT :: Word32
pattern VK_SHADER_STAGE_FRAGMENT_BIT = 0x00000010

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER :: Word32
pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = 1

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC :: Word32
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = 8

pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT :: Word32
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = 0x00000001

pattern VK_FILTER_LINEAR :: Word32
pattern VK_FILTER_LINEAR = 1

pattern VK_FILTER_NEAREST :: Word32
pattern VK_FILTER_NEAREST = 0

pattern VK_SAMPLER_MIPMAP_MODE_NEAREST :: Word32
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = 0

pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE :: Word32
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = 2

pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK :: Word32
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = 0

pattern VK_QUERY_TYPE_TIMESTAMP :: Word32
pattern VK_QUERY_TYPE_TIMESTAMP = 2

pattern VK_QUERY_RESULT_64_BIT :: Word32
pattern VK_QUERY_RESULT_64_BIT = 0x00000001

pattern VK_QUERY_RESULT_WAIT_BIT :: Word32
pattern VK_QUERY_RESULT_WAIT_BIT = 0x00000002
