{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Core (

	-- * INFO

	-- ** ApplicationInfo

	ApplicationInfo, PtrApplicationInfo, pattern ApplicationInfo,
	applicationInfoSType, applicationInfoPNext,
	applicationInfoPApplicationName, applicationInfoApplicationVersion,
	applicationInfoPEngineName, applicationInfoEngineVersion,
	applicationInfoApiVersion,

	-- *** ApiVersion

	ApiVersion, makeApiVersion,
	apiVersion_1_0, apiVersion_1_1, apiVersion_1_2, apiVersion_1_3,

	-- ** SubmitInfo

	SubmitInfo, pattern SubmitInfo,
	submitInfoSType, submitInfoPNext,
	submitInfoWaitSemaphoreCount, submitInfoPWaitSemaphores,
	submitInfoPWaitDstStageMask, PtrPipelineStageFlags,
	submitInfoCommandBufferCount, submitInfoPCommandBuffers,
	submitInfoSignalSemaphoreCount, submitInfoPSignalSemaphores,

	SubmitInfo2, pattern SubmitInfo2,
	submitInfo2SType, submitInfo2PNext, submitInfo2Flags,
	submitInfo2WaitSemaphoreInfoCount, submitInfo2PWaitSemaphoreInfos,
	submitInfo2CommandBufferInfoCount, submitInfo2PCommandBufferInfos,
	submitInfo2SignalSemaphoreInfoCount, submitInfo2PSignalSemaphoreInfos,

	-- * PROPERTIES

	-- ** ExtensionProperties

	ExtensionProperties, pattern ExtensionProperties,
	extensionPropertiesExtensionName, extensionPropertiesSpecVersion,

	-- ** LayerProperties

	LayerProperties, pattern LayerProperties,
	layerPropertiesLayerName, layerPropertiesSpecVersion,
	layerPropertiesImplementationVersion, layerPropertiesDescription,

	-- ** FormatProperties

	FormatProperties, pattern FormatProperties,
	formatPropertiesLinearTilingFeatures,
	formatPropertiesOptimalTilingFeatures, formatPropertiesBufferFeatures,

	-- * PIPELINE VALUES

	-- ** Viewport

	Viewport, PtrViewport, pattern Viewport,
	viewportX, viewportY, viewportWidth, viewportHeight,
	viewportMinDepth, viewportMaxDepth,

	-- ** StencilOpState

	StencilOpState, pattern StencilOpState,
	stencilOpStateFailOp, stencilOpStatePassOp, stencilOpStateDepthFailOp,
	stencilOpStateCompareOp, stencilOpStateCompareMask,
	stencilOpStateWriteMask, stencilOpStateReference,

	-- ** ClearValue

	ClearValue, PtrClearValue,
	clearValueFromClearColorValue, clearValueFromClearDepthStencilValue,

	-- *** ClearColorValue

	ClearColorValue,
	clearColorValueFromUints, clearColorValueFromInts,
	clearColorValueFromFloats,

	-- *** ClearDepthStencilValue
	
	ClearDepthStencilValue, pattern ClearDepthStencilValue,
	clearDepthStencilValueDepth, clearDepthStencilValueStencil,

	-- * STRUCT COMMON

	StructCommon, pattern StructCommon,
	structCommonSType, structCommonPNext,

	-- * RECT, OFFSET AND EXTENT

	-- ** Rect

	Rect2d, PtrRect2d, pattern Rect2d,
	rect2dExtent, rect2dOffset,

	-- ** Offset

	Offset2d, pattern Offset2d,
	offset2dX, offset2dY,

	Offset3d, ListOffset3d, pattern Offset3d,
	offset3dX, offset3dY, offset3dZ,

	-- ** Extent

	Extent2d, pattern Extent2d,
	extent2dWidth, extent2dHeight,

	Extent3d, pattern Extent3d,
	extent3dWidth, extent3dHeight, extent3dDepth,

	-- ** DEPENDENCY INFO

	DependencyInfo, pattern DependencyInfo,
	dependencyInfoSType, dependencyInfoPNext, dependencyInfoDependencyFlags,
	dependencyInfoMemoryBarrierCount, dependencyInfoPMemoryBarriers,
	dependencyInfoBufferMemoryBarrierCount,
	dependencyInfoPBufferMemoryBarriers,
	dependencyInfoImageMemoryBarrierCount,
	dependencyInfoPImageMemoryBarriers,

	-- * BLIT IMAGE INFO 2

	BlitImageInfo2, pattern BlitImageInfo2,
	blitImageInfo2SType, blitImageInfo2PNext,
	blitImageInfo2SrcImage, blitImageInfo2SrcImageLayout,
	blitImageInfo2DstImage, blitImageInfo2DstImageLayout,
	blitImageInfo2RegionCount, blitImageInfo2PRegions, blitImageInfo2Filter

	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Data.Text as Txt
import qualified Data.Text.Foreign as Txt

import {-# SOURCE #-} qualified Gpu.Vulkan.CommandBuffer.Core as CommandBuffer
import {-# SOURCE #-} qualified Gpu.Vulkan.Semaphore.Core as Semaphore

import qualified Gpu.Vulkan.Memory.Core as Memory
import {-# SOURCE #-} qualified Gpu.Vulkan.Buffer.Core as Buffer
import {-# SOURCE #-} qualified Gpu.Vulkan.Image.Core as Image

#include <vulkan/vulkan.h>

struct "StructCommon" #{size VkApplicationInfo}
		#{alignment VkApplicationInfo} [
	("sType", ''#{type VkStructureType},
		[| #{peek VkApplicationInfo, sType} |],
		[| #{poke VkApplicationInfo, sType} |] ),
	("pNext", ''PtrVoid,
		[| #{peek VkApplicationInfo, pNext} |],
		[| #{poke VkApplicationInfo, pNext} |]) ]
	[''Show, ''Storable]

type ApiVersion = #{type uint32_t}

struct "ApplicationInfo" #{size VkApplicationInfo}
		#{alignment VkApplicationInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkApplicationInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_APPLICATION_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkApplicationInfo, pNext} |],
		[| #{poke VkApplicationInfo, pNext} |]),
	("pApplicationName", ''CString,
		[| #{peek VkApplicationInfo, pApplicationName} |],
		[| #{poke VkApplicationInfo, pApplicationName} |]),
	("applicationVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, applicationVersion} |],
		[| #{poke VkApplicationInfo, applicationVersion} |]),
	("pEngineName", ''CString,
		[| #{peek VkApplicationInfo, pEngineName} |],
		[| #{poke VkApplicationInfo, pEngineName} |]),
	("engineVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, engineVersion} |],
		[| #{poke VkApplicationInfo, engineVersion} |]),
	("apiVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, apiVersion} |],
		[| #{poke VkApplicationInfo, apiVersion} |]) ]
	[''Show, ''Storable]

foreign import capi "vulkan/vulkan.h VK_MAKE_API_VERSION" makeApiVersion ::
	Word8 -> Word8 -> Word16 -> Word16 -> ApiVersion

foreign import capi "vulkan/vulkan.h value VK_API_VERSION_1_0" apiVersion_1_0 ::
	ApiVersion

foreign import capi "vulkan/vulkan.h value VK_API_VERSION_1_1" apiVersion_1_1 ::
	ApiVersion

foreign import capi "vulkan/vulkan.h value VK_API_VERSION_1_2" apiVersion_1_2 ::
	ApiVersion

foreign import capi "vulkan/vulkan.h value VK_API_VERSION_1_3" apiVersion_1_3 ::
	ApiVersion

type PtrApplicationInfo = Ptr ApplicationInfo

struct "Extent2d" #{size VkExtent2D} #{alignment VkExtent2D} [
	("width", ''#{type uint32_t}, [| #{peek VkExtent2D, width} |],
		[| #{poke VkExtent2D, width} |]),
	("height", ''#{type uint32_t}, [| #{peek VkExtent2D, height} |],
		[| #{poke VkExtent2D, height} |]) ]
	[''Show, ''Storable]

struct "Extent3d" #{size VkExtent3D} #{alignment VkExtent3D} [
	("width", ''#{type uint32_t}, [| #{peek VkExtent3D, width} |],
		[| #{poke VkExtent3D, width} |]),
	("height", ''#{type uint32_t}, [| #{peek VkExtent3D, height} |],
		[| #{poke VkExtent3D, height} |]),
	("depth", ''#{type uint32_t}, [| #{peek VkExtent3D, depth} |],
		[| #{poke VkExtent3D, depth} |]) ]
	[''Show, ''Storable]

struct "Viewport" #{size VkViewport} #{alignment VkViewport} [
	("x", ''#{type float}, [| #{peek VkViewport, x} |],
		[| #{poke VkViewport, x} |]),
	("y", ''#{type float}, [| #{peek VkViewport, y} |],
		[| #{poke VkViewport, y} |]),
	("width", ''#{type float}, [| #{peek VkViewport, width} |],
		[| #{poke VkViewport, width} |]),
	("height", ''#{type float}, [| #{peek VkViewport, height} |],
		[| #{poke VkViewport, height} |]),
	("minDepth", ''#{type float}, [| #{peek VkViewport, minDepth} |],
		[| #{poke VkViewport, minDepth} |]),
	("maxDepth", ''#{type float}, [| #{peek VkViewport, maxDepth} |],
		[| #{poke VkViewport, maxDepth} |]) ]
	[''Show, ''Storable]

type PtrViewport = Ptr Viewport

struct "Offset2d" #{size VkOffset2D} #{alignment VkOffset2D} [
	("x", ''#{type int32_t}, [| #{peek VkOffset2D, x} |],
		[| #{poke VkOffset2D, x} |]),
	("y", ''#{type int32_t}, [| #{peek VkOffset2D, y} |],
		[| #{poke VkOffset2D, y} |]) ]
	[''Show, ''Storable]

struct "Offset3d" #{size VkOffset3D} #{alignment VkOffset3D} [
	("x", ''#{type int32_t}, [| #{peek VkOffset3D, x} |],
		[| #{poke VkOffset3D, x} |]),
	("y", ''#{type int32_t}, [| #{peek VkOffset3D, y} |],
		[| #{poke VkOffset3D, y} |]),
	("z", ''#{type int32_t}, [| #{peek VkOffset3D, z} |],
		[| #{poke VkOffset3D, z} |]) ]
	[''Show, ''Storable]

type ListOffset3d = [Offset3d]

struct "Rect2d" #{size VkRect2D} #{alignment VkRect2D} [
	("offset", ''Offset2d, [| #{peek VkRect2D, offset} |],
		[| #{poke VkRect2D, offset} |]),
	("extent", ''Extent2d, [| #{peek VkRect2D, extent} |],
		[| #{poke VkRect2D, extent} |]) ]
	[''Show, ''Storable]

type PtrRect2d = Ptr Rect2d

type PtrPipelineStageFlags = Ptr #{type VkPipelineStageFlags}

sTypeS :: #{type VkStructureType}
sTypeS = #{const VK_STRUCTURE_TYPE_SUBMIT_INFO}

struct "SubmitInfo" #{size VkSubmitInfo} #{alignment VkSubmitInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSubmitInfo, sType} p sTypeS |]),
	("pNext", ''PtrVoid,
		[| #{peek VkSubmitInfo, pNext} |],
		[| #{poke VkSubmitInfo, pNext} |]),
	("waitSemaphoreCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo, waitSemaphoreCount} |],
		[| #{poke VkSubmitInfo, waitSemaphoreCount} |]),
	("pWaitSemaphores", ''Semaphore.PtrS,
		[| #{peek VkSubmitInfo, pWaitSemaphores} |],
		[| #{poke VkSubmitInfo, pWaitSemaphores} |]),
	("pWaitDstStageMask", ''PtrPipelineStageFlags,
		[| #{peek VkSubmitInfo, pWaitDstStageMask} |],
		[| #{poke VkSubmitInfo, pWaitDstStageMask} |]),
	("commandBufferCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo, commandBufferCount} |],
		[| #{poke VkSubmitInfo, commandBufferCount} |]),
	("pCommandBuffers", ''CommandBuffer.PtrC,
		[| #{peek VkSubmitInfo, pCommandBuffers} |],
		[| #{poke VkSubmitInfo, pCommandBuffers} |]),
	("signalSemaphoreCount", ''#{type int32_t},
		[| #{peek VkSubmitInfo, signalSemaphoreCount} |],
		[| #{poke VkSubmitInfo, signalSemaphoreCount} |]),
	("pSignalSemaphores", ''Semaphore.PtrS,
		[| #{peek VkSubmitInfo, pSignalSemaphores} |],
		[| #{poke VkSubmitInfo, pSignalSemaphores} |]) ]
	[''Show, ''Storable]

sTypeS2 :: #{type VkStructureType}
sTypeS2 = #{const VK_STRUCTURE_TYPE_SUBMIT_INFO_2}

struct "SubmitInfo2" #{size VkSubmitInfo2} #{alignment VkSubmitInfo2} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSubmitInfo2, sType} p sTypeS2 |]),
	("pNext", ''PtrVoid,
		[| #{peek VkSubmitInfo2, pNext} |],
		[| #{poke VkSubmitInfo2, pNext} |]),
	("flags", ''#{type VkSubmitFlags},
		[| #{peek VkSubmitInfo2, flags} |],
		[| #{poke VkSubmitInfo2, flags} |]),
	("waitSemaphoreInfoCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo2, waitSemaphoreInfoCount} |],
		[| #{poke VkSubmitInfo2, waitSemaphoreInfoCount} |]),
	("pWaitSemaphoreInfos", ''Semaphore.PtrSubmitInfo,
		[| #{peek VkSubmitInfo2, pWaitSemaphoreInfos} |],
		[| #{poke VkSubmitInfo2, pWaitSemaphoreInfos} |]),
	("commandBufferInfoCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo2, commandBufferInfoCount} |],
		[| #{poke VkSubmitInfo2, commandBufferInfoCount} |]),
	("pCommandBufferInfos", ''CommandBuffer.PtrSubmitInfo,
		[| #{peek VkSubmitInfo2, pCommandBufferInfos} |],
		[| #{poke VkSubmitInfo2, pCommandBufferInfos} |]),
	("signalSemaphoreInfoCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo2, signalSemaphoreInfoCount} |],
		[| #{poke VkSubmitInfo2, signalSemaphoreInfoCount} |]),
	("pSignalSemaphoreInfos", ''Semaphore.PtrSubmitInfo,
		[| #{peek VkSubmitInfo2, pSignalSemaphoreInfos} |],
		[| #{poke VkSubmitInfo2, pSignalSemaphoreInfos} |]) ]
	[''Show, ''Storable]

struct "ExtensionProperties" #{size VkExtensionProperties}
		#{alignment VkExtensionProperties} [
	("extensionName", ''Txt.Text,
		[| \p -> Txt.takeWhile (/= '\NUL') <$> Txt.peekCStringLen
			(#{ptr VkExtensionProperties, extensionName} p,
				#{const VK_MAX_EXTENSION_NAME_SIZE}) |],
		[| \p bs -> Txt.withCStringLen bs \(cs, ln) -> do
			copyBytes (#{ptr VkExtensionProperties, extensionName} p) cs ln
			poke (#{ptr VkExtensionProperties, extensionName} p `plusPtr` ln :: Ptr CChar) 0
			|]
		),
	("specVersion", ''#{type uint32_t},
		[| #{peek VkExtensionProperties, specVersion} |],
		[| #{poke VkExtensionProperties, specVersion} |]) ]
	[''Show, ''Storable]

struct "LayerProperties" #{size VkLayerProperties}
		#{alignment VkLayerProperties} [
	("layerName", ''Txt.Text,
		[| \p -> Txt.takeWhile (/= '\NUL') <$> Txt.peekCStringLen
			(#{ptr VkLayerProperties, layerName} p,
				#{const VK_MAX_EXTENSION_NAME_SIZE}) |],
		[| \p bs -> Txt.withCStringLen bs \(cs, ln) -> do
			copyBytes (#{ptr VkLayerProperties, layerName} p) cs ln
			poke (#{ptr VkLayerProperties, layerName} p `plusPtr` ln :: Ptr CChar) 0
			|]),
	("specVersion", ''#{type uint32_t},
		[| #{peek VkLayerProperties, specVersion} |],
		[| #{poke VkLayerProperties, specVersion} |]),
	("implementationVersion", ''#{type uint32_t},
		[| #{peek VkLayerProperties, implementationVersion} |],
		[| #{poke VkLayerProperties, implementationVersion} |]),
	("description", ''Txt.Text,
		[| \p -> Txt.takeWhile (/= '\NUL') <$> Txt.peekCStringLen
			(#{ptr VkLayerProperties, description} p,
				#{const VK_MAX_DESCRIPTION_SIZE}) |],
		[| \p bs -> Txt.withCStringLen bs \(cs, ln) -> do
			copyBytes (#{ptr VkLayerProperties, description} p) cs ln
			poke (#{ptr VkLayerProperties, description} p `plusPtr` ln :: Ptr CChar) 0
			|]) ]
	[''Show, ''Storable]

struct "StencilOpState" #{size VkStencilOpState} #{alignment VkStencilOpState} [
	("failOp", ''#{type VkStencilOp},
		[| #{peek VkStencilOpState, failOp} |],
		[| #{poke VkStencilOpState, failOp} |]),
	("passOp", ''#{type VkStencilOp},
		[| #{peek VkStencilOpState, passOp} |],
		[| #{poke VkStencilOpState, passOp} |]),
	("depthFailOp", ''#{type VkStencilOp},
		[| #{peek VkStencilOpState, depthFailOp} |],
		[| #{poke VkStencilOpState, depthFailOp} |]),
	("compareOp", ''#{type VkCompareOp},
		[| #{peek VkStencilOpState, compareOp} |],
		[| #{poke VkStencilOpState, compareOp} |]),
	("compareMask", ''#{type uint32_t},
		[| #{peek VkStencilOpState, compareMask} |],
		[| #{poke VkStencilOpState, compareMask} |]),
	("writeMask", ''#{type uint32_t},
		[| #{peek VkStencilOpState, writeMask} |],
		[| #{poke VkStencilOpState, writeMask} |]),
	("reference", ''#{type uint32_t},
		[| #{peek VkStencilOpState, reference} |],
		[| #{poke VkStencilOpState, reference} |]) ]
	[''Show, ''Storable]

data ClearValue
type PtrClearValue = Ptr ClearValue
data ClearColorValue

clearColorValueFromFloats :: Ptr #{type float} -> Ptr ClearColorValue
clearColorValueFromFloats = castPtr

clearColorValueFromInts :: Ptr #{type int32_t} -> Ptr ClearColorValue
clearColorValueFromInts = castPtr

clearColorValueFromUints :: Ptr #{type uint32_t} -> Ptr ClearColorValue
clearColorValueFromUints = castPtr

struct "ClearDepthStencilValue" #{size VkClearDepthStencilValue}
		#{alignment VkClearDepthStencilValue} [
	("depth", ''#{type float},
		[| #{peek VkClearDepthStencilValue, depth} |],
		[| #{poke VkClearDepthStencilValue, depth} |]),
	("stencil", ''#{type uint32_t},
		[| #{peek VkClearDepthStencilValue, stencil} |],
		[| #{poke VkClearDepthStencilValue, stencil} |]) ]
	[''Show, ''Storable]

clearValueFromClearColorValue :: Ptr ClearColorValue -> Ptr ClearValue
clearValueFromClearColorValue = castPtr

clearValueFromClearDepthStencilValue ::
	ClearDepthStencilValue -> (Ptr ClearValue -> IO a) -> IO a
clearValueFromClearDepthStencilValue (ClearDepthStencilValue_ fp) f =
	withForeignPtr fp $ f . castPtr

struct "FormatProperties" #{size VkFormatProperties}
		#{alignment VkFormatProperties} [
	("linearTilingFeatures", ''#{type VkFormatFeatureFlags},
		[| #{peek VkFormatProperties, linearTilingFeatures} |],
		[| #{poke VkFormatProperties, linearTilingFeatures} |]),
	("optimalTilingFeatures", ''#{type VkFormatFeatureFlags},
		[| #{peek VkFormatProperties, optimalTilingFeatures} |],
		[| #{poke VkFormatProperties, optimalTilingFeatures} |]),
	("bufferFeatures", ''#{type VkFormatFeatureFlags},
		[| #{peek VkFormatProperties, bufferFeatures} |],
		[| #{poke VkFormatProperties, bufferFeatures} |]) ]
	[''Show, ''Storable]

struct "DependencyInfo" #{size VkDependencyInfo} #{alignment VkDependencyInfo} [
	("sType",  ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkApplicationInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_DEPENDENCY_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDependencyInfo, pNext} |],
		[| #{poke VkDependencyInfo, pNext} |]),
	("dependencyFlags", ''#{type VkDependencyFlags},
		[| #{peek VkDependencyInfo, dependencyFlags} |],
		[| #{poke VkDependencyInfo, dependencyFlags} |] ),
	("memoryBarrierCount", ''#{type uint32_t},
		[| #{peek VkDependencyInfo, memoryBarrierCount} |],
		[| #{poke VkDependencyInfo, memoryBarrierCount} |] ),
	("pMemoryBarriers", ''Memory.PtrBarrier2,
		[| #{peek VkDependencyInfo, pMemoryBarriers} |],
		[| #{poke VkDependencyInfo, pMemoryBarriers} |] ),
	("bufferMemoryBarrierCount", ''#{type uint32_t},
		[| #{peek VkDependencyInfo, bufferMemoryBarrierCount} |],
		[| #{poke VkDependencyInfo, bufferMemoryBarrierCount} |]),
	("pBufferMemoryBarriers", ''Buffer.PtrMemoryBarrier2,
		[| #{peek VkDependencyInfo, pBufferMemoryBarriers} |],
		[| #{poke VkDependencyInfo, pBufferMemoryBarriers} |]),
	("imageMemoryBarrierCount", ''#{type uint32_t},
		[| #{peek VkDependencyInfo, imageMemoryBarrierCount} |],
		[| #{poke VkDependencyInfo, imageMemoryBarrierCount} |]),
	("pImageMemoryBarriers", ''Image.PtrMemoryBarrier2,
		[| #{peek VkDependencyInfo, pImageMemoryBarriers} |],
		[| #{poke VkDependencyInfo, pImageMemoryBarriers} |]) ]
	[''Show, ''Storable]

struct "BlitImageInfo2" #{size VkBlitImageInfo2} #{alignment VkBlitImageInfo2} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkBlitImageInfo2, sType} p
			(#{const VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid,
		[| #{peek VkBlitImageInfo2, pNext} |],
		[| #{poke VkBlitImageInfo2, pNext} |]),
	("srcImage", ''Image.I,
		[| #{peek VkBlitImageInfo2, srcImage} |],
		[| #{poke VkBlitImageInfo2, srcImage} |]),
	("srcImageLayout", ''#{type VkImageLayout},
		[| #{peek VkBlitImageInfo2, srcImageLayout} |],
		[| #{poke VkBlitImageInfo2, srcImageLayout} |]),
	("dstImage", ''Image.I,
		[| #{peek VkBlitImageInfo2, dstImage} |],
		[| #{poke VkBlitImageInfo2, dstImage} |]),
	("dstImageLayout", ''#{type VkImageLayout},
		[| #{peek VkBlitImageInfo2, dstImageLayout} |],
		[| #{poke VkBlitImageInfo2, dstImageLayout} |]),
	("regionCount", ''#{type uint32_t},
		[| #{peek VkBlitImageInfo2, regionCount} |],
		[| #{poke VkBlitImageInfo2, regionCount} |]),
	("pRegions", ''Image.PtrBlit2,
		[| #{peek VkBlitImageInfo2, pRegions} |],
		[| #{poke VkBlitImageInfo2, pRegions} |]),
	("filter", ''#{type VkFilter},
		[| #{peek VkBlitImageInfo2, filter} |],
		[| #{poke VkBlitImageInfo2, filter} |]) ]
	[''Show, ''Storable]
