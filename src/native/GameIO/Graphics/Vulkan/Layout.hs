-- Layouts derived from the C declarations in Vulkan-Headers v1.4.341 on Win64.
module GameIO.Graphics.Vulkan.Layout where

import Control.Monad (forM_)
import qualified Data.List as List
import Foreign hiding (alignment)

data Field
  = U32 !Word32
  | I32 !Int32
  | U64 !Word64
  | F32 !Float
  | Pointer !(Ptr ())
  | Aggregate !Structure

data Structure = Structure !Abi ![(Int, Field)]

data Abi = Abi
  { abiSize :: !Int
  , abiAlignment :: !Int
  } deriving (Eq, Show)

pointer :: Ptr a -> Field
pointer = Pointer . castPtr

structure :: [Field] -> Structure
structure members = Structure (Abi (aligned lastByte boundary) boundary) (reverse offsets)
  where
    (lastByte, boundary, offsets) = List.foldl' place (0, 1, []) members
    place (end, widest, placed) field =
      let Abi bytes alignment = fieldAbi field
          offset = aligned end alignment
      in (offset + bytes, max widest alignment, (offset, field) : placed)

fieldAbi :: Field -> Abi
fieldAbi field = case field of
  U32 _ -> Abi 4 4
  I32 _ -> Abi 4 4
  U64 _ -> Abi 8 8
  F32 _ -> Abi 4 4
  Pointer _ -> Abi 8 8
  Aggregate (Structure abi _) -> abi

aligned :: Int -> Int -> Int
aligned bytes alignment = (bytes + alignment - 1) `div` alignment * alignment

withStruct :: Structure -> (Ptr () -> IO a) -> IO a
withStruct value@(Structure abi _) use = withOutput abi $ \memory -> do
  writeStruct memory value
  use memory

withStructArray :: Structure -> [Structure] -> (Ptr () -> IO a) -> IO a
withStructArray first@(Structure abi _) rest use =
  withOutput (abi { abiSize = abiSize abi * (1 + length rest) }) $ \memory -> do
    forM_ (zip [0..] (first : rest)) $ \(index, element) ->
      writeStruct (memory `plusPtr` (index * abiSize abi)) element
    use memory

withOutput :: Abi -> (Ptr () -> IO a) -> IO a
withOutput (Abi bytes alignment) use =
  allocaBytesAligned bytes alignment $ \memory -> fillBytes memory 0 bytes >> use memory

writeStruct :: Ptr () -> Structure -> IO ()
writeStruct memory (Structure _ members) = forM_ members $ \(offset, field) ->
  case field of
    U32 value -> pokeByteOff memory offset value
    I32 value -> pokeByteOff memory offset value
    U64 value -> pokeByteOff memory offset value
    F32 value -> pokeByteOff memory offset value
    Pointer value -> pokeByteOff memory offset value
    Aggregate value -> writeStruct (memory `plusPtr` offset) value

withInfo :: Word32 -> [Field] -> (Ptr () -> IO a) -> IO a
withInfo kind = withInfoNext kind nullPtr

withInfoNext :: Word32 -> Ptr () -> [Field] -> (Ptr () -> IO a) -> IO a
withInfoNext kind next members = withStruct (info kind next members)

info :: Word32 -> Ptr () -> [Field] -> Structure
info kind next members = structure (U32 kind : pointer next : members)

words32 :: [Word32] -> Field
words32 = Aggregate . structure . map U32

floats :: [Float] -> Field
floats = Aggregate . structure . map F32

extent2D :: Word32 -> Word32 -> Field
extent2D width height = words32 [width, height]

rect2D :: Int32 -> Int32 -> Word32 -> Word32 -> Structure
rect2D x y width height = structure [I32 x, I32 y, extent2D width height]

-- VkPhysicalDeviceFeatures2
physicalDeviceFeatures2Abi :: Abi
physicalDeviceFeatures2Abi = Abi 240 8
physicalDeviceFeatures2_features :: Int
physicalDeviceFeatures2_features = 16

-- VkPhysicalDeviceDynamicRenderingFeatures
physicalDeviceDynamicRenderingFeaturesAbi :: Abi
physicalDeviceDynamicRenderingFeaturesAbi = Abi 24 8
physicalDeviceDynamicRenderingFeatures_dynamicRendering :: Int
physicalDeviceDynamicRenderingFeatures_dynamicRendering = 16

-- VkPhysicalDeviceSwapchainMaintenance1FeaturesKHR
physicalDeviceSwapchainMaintenance1FeaturesKHRAbi :: Abi
physicalDeviceSwapchainMaintenance1FeaturesKHRAbi = Abi 24 8
physicalDeviceSwapchainMaintenance1FeaturesKHR_swapchainMaintenance1 :: Int
physicalDeviceSwapchainMaintenance1FeaturesKHR_swapchainMaintenance1 = 16

-- VkPhysicalDeviceProperties
physicalDevicePropertiesAbi :: Abi
physicalDevicePropertiesAbi = Abi 824 8
physicalDeviceProperties_apiVersion :: Int
physicalDeviceProperties_apiVersion = 0
physicalDeviceProperties_deviceName :: Int
physicalDeviceProperties_deviceName = 20

-- VkQueueFamilyProperties
queueFamilyPropertiesAbi :: Abi
queueFamilyPropertiesAbi = Abi 24 4
queueFamilyProperties_queueFlags :: Int
queueFamilyProperties_queueFlags = 0
queueFamilyProperties_queueCount :: Int
queueFamilyProperties_queueCount = 4
queueFamilyProperties_timestampValidBits :: Int
queueFamilyProperties_timestampValidBits = 8

-- VkPhysicalDeviceMemoryProperties
physicalDeviceMemoryPropertiesAbi :: Abi
physicalDeviceMemoryPropertiesAbi = Abi 520 8
physicalDeviceMemoryProperties_memoryTypeCount :: Int
physicalDeviceMemoryProperties_memoryTypeCount = 0
physicalDeviceMemoryProperties_memoryTypes :: Int
physicalDeviceMemoryProperties_memoryTypes = 4

-- VkMemoryType
memoryTypeAbi :: Abi
memoryTypeAbi = Abi 8 4
memoryType_propertyFlags :: Int
memoryType_propertyFlags = 0

-- VkExtensionProperties
extensionPropertiesAbi :: Abi
extensionPropertiesAbi = Abi 260 4
extensionProperties_extensionName :: Int
extensionProperties_extensionName = 0

-- VkFormatProperties
formatPropertiesAbi :: Abi
formatPropertiesAbi = Abi 12 4
formatProperties_optimalTilingFeatures :: Int
formatProperties_optimalTilingFeatures = 4

-- VkSurfaceCapabilitiesKHR
surfaceCapabilitiesKHRAbi :: Abi
surfaceCapabilitiesKHRAbi = Abi 52 4
surfaceCapabilitiesKHR_minImageCount :: Int
surfaceCapabilitiesKHR_minImageCount = 0
surfaceCapabilitiesKHR_maxImageCount :: Int
surfaceCapabilitiesKHR_maxImageCount = 4
surfaceCapabilitiesKHR_currentExtent :: Int
surfaceCapabilitiesKHR_currentExtent = 8
surfaceCapabilitiesKHR_minImageExtent :: Int
surfaceCapabilitiesKHR_minImageExtent = 16
surfaceCapabilitiesKHR_maxImageExtent :: Int
surfaceCapabilitiesKHR_maxImageExtent = 24
surfaceCapabilitiesKHR_currentTransform :: Int
surfaceCapabilitiesKHR_currentTransform = 40
surfaceCapabilitiesKHR_supportedCompositeAlpha :: Int
surfaceCapabilitiesKHR_supportedCompositeAlpha = 44
surfaceCapabilitiesKHR_supportedUsageFlags :: Int
surfaceCapabilitiesKHR_supportedUsageFlags = 48

-- VkSurfaceFormatKHR
surfaceFormatKHRAbi :: Abi
surfaceFormatKHRAbi = Abi 8 4
surfaceFormatKHR_format :: Int
surfaceFormatKHR_format = 0
surfaceFormatKHR_colorSpace :: Int
surfaceFormatKHR_colorSpace = 4

-- VkMemoryRequirements
memoryRequirementsAbi :: Abi
memoryRequirementsAbi = Abi 24 8
memoryRequirements_size :: Int
memoryRequirements_size = 0
memoryRequirements_memoryTypeBits :: Int
memoryRequirements_memoryTypeBits = 16

-- These fields are nested in VkPhysicalDeviceProperties.limits.
physicalDeviceProperties_timestampPeriod, physicalDeviceProperties_maxImageDimension2D :: Int
physicalDeviceProperties_timestampPeriod = 720
physicalDeviceProperties_maxImageDimension2D = 300

physicalDeviceProperties_minUniformBufferOffsetAlignment, physicalDeviceProperties_maxBoundDescriptorSets :: Int
physicalDeviceProperties_minUniformBufferOffsetAlignment = 616
physicalDeviceProperties_maxBoundDescriptorSets = 360
