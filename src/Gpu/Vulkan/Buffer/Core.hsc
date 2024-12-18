{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer.Core (

	-- * CREATE AND DESTROY

	create, destroy, getMemoryRequirements, bindMemory, B,
	CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags, createInfoSize,
	createInfoUsage, createInfoSharingMode,
	createInfoQueueFamilyIndexCount, createInfoPQueueFamilyIndices,

	-- * COPY

	Copy, pattern Copy,
	copySrcOffset, copyDstOffset, copySize,

	ImageCopy, pattern ImageCopy,
	imageCopyBufferOffset,
	imageCopyBufferRowLength, imageCopyBufferImageHeight,
	imageCopyImageSubresource, imageCopyImageOffset, imageCopyImageExtent,

	-- * MEMORY BARRIER

	MemoryBarrier, pattern MemoryBarrier,
	memoryBarrierSType, memoryBarrierPNext,
	memoryBarrierSrcAccessMask, memoryBarrierDstAccessMask,
	memoryBarrierSrcQueueFamilyIndex, memoryBarrierDstQueueFamilyIndex,
	memoryBarrierBuffer, memoryBarrierOffset, memoryBarrierSize,

	MemoryBarrier2, PtrMemoryBarrier2, pattern MemoryBarrier2,
	memoryBarrier2SType, memoryBarrier2PNext,
	memoryBarrier2SrcStageMask, memoryBarrier2SrcAccessMask,
	memoryBarrier2DstStageMask, memoryBarrier2DstAccessMask,
	memoryBarrier2SrcQueueFamilyIndex, memoryBarrier2DstQueueFamilyIndex,
	memoryBarrier2Buffer, memoryBarrier2Offset, memoryBarrier2Size

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import Gpu.Vulkan.Core
import Gpu.Vulkan.TypeSynonyms.Core
import Gpu.Vulkan.AllocationCallbacks.Core qualified as AllocationCallbacks
import {-# SOURCE #-} Gpu.Vulkan.Device.Core qualified as Device
import Gpu.Vulkan.Memory.Core qualified as Memory
import Gpu.Vulkan.Image.Core qualified as Image

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkBufferCreateInfo} #{alignment VkBufferCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkBufferCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkBufferCreateInfo, pNext} |],
		[| #{poke VkBufferCreateInfo, pNext} |]),
	("flags", ''#{type VkBufferCreateFlags},
		[| #{peek VkBufferCreateInfo, flags} |],
		[| #{poke VkBufferCreateInfo, flags} |]),
	("size", ''#{type VkDeviceSize},
		[| #{peek VkBufferCreateInfo, size} |],
		[| #{poke VkBufferCreateInfo, size} |]),
	("usage", ''#{type VkBufferUsageFlags},
		[| #{peek VkBufferCreateInfo, usage} |],
		[| #{poke VkBufferCreateInfo, usage} |]),
	("sharingMode", ''#{type VkSharingMode},
		[| #{peek VkBufferCreateInfo, sharingMode} |],
		[| #{poke VkBufferCreateInfo, sharingMode} |]),
	("queueFamilyIndexCount", ''#{type uint32_t},
		[| #{peek VkBufferCreateInfo, queueFamilyIndexCount} |],
		[| #{poke VkBufferCreateInfo, queueFamilyIndexCount} |]),
	("pQueueFamilyIndices", ''PtrUint32T,
		[| #{peek VkBufferCreateInfo, pQueueFamilyIndices} |],
		[| #{poke VkBufferCreateInfo, pQueueFamilyIndices} |]) ]
	[''Show, ''Storable]

data BTag
type B = Ptr BTag

foreign import ccall "vkCreateBuffer" create ::
	Device.D -> Ptr CreateInfo ->  Ptr AllocationCallbacks.A -> Ptr B ->
	IO #{type VkResult}

foreign import ccall "vkDestroyBuffer" destroy ::
	Device.D -> B -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkGetBufferMemoryRequirements" getMemoryRequirements ::
	Device.D -> B -> Ptr Memory.Requirements -> IO ()

foreign import ccall "vkBindBufferMemory" bindMemory ::
	Device.D -> B -> Memory.M -> #{type VkDeviceSize} ->
	IO #{type VkResult}

struct "Copy" #{size VkBufferCopy} #{alignment VkBufferCopy} [
	("srcOffset", ''#{type VkDeviceSize},
		[| #{peek VkBufferCopy, srcOffset} |],
		[| #{poke VkBufferCopy, srcOffset} |]),
	("dstOffset", ''#{type VkDeviceSize},
		[| #{peek VkBufferCopy, dstOffset} |],
		[| #{poke VkBufferCopy, dstOffset} |]),
	("size", ''#{type VkDeviceSize},
		[| #{peek VkBufferCopy, size} |],
		[| #{poke VkBufferCopy, size} |]) ]
	[''Show, ''Storable]

mbType :: #{type VkStructureType}
mbType = #{const VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER}

struct "MemoryBarrier" #{size VkBufferMemoryBarrier}
		#{alignment VkBufferMemoryBarrier} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkBufferMemoryBarrier, sType} p mbType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkBufferMemoryBarrier, pNext} |],
		[| #{poke VkBufferMemoryBarrier, pNext} |]),
	("srcAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkBufferMemoryBarrier, srcAccessMask} |],
		[| #{poke VkBufferMemoryBarrier, srcAccessMask} |]),
	("dstAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkBufferMemoryBarrier, dstAccessMask} |],
		[| #{poke VkBufferMemoryBarrier, dstAccessMask} |]),
	("srcQueueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkBufferMemoryBarrier, srcQueueFamilyIndex} |],
		[| #{poke VkBufferMemoryBarrier, srcQueueFamilyIndex} |]),
	("dstQueueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkBufferMemoryBarrier, dstQueueFamilyIndex} |],
		[| #{poke VkBufferMemoryBarrier, dstQueueFamilyIndex} |]),
	("buffer", ''B,
		[| #{peek VkBufferMemoryBarrier, buffer} |],
		[| #{poke VkBufferMemoryBarrier, buffer} |]),
	("offset", ''#{type VkDeviceSize},
		[| #{peek VkBufferMemoryBarrier, offset} |],
		[| #{poke VkBufferMemoryBarrier, offset} |]),
	("size", ''#{type VkDeviceSize},
		[| #{peek VkBufferMemoryBarrier, size} |],
		[| #{poke VkBufferMemoryBarrier, size} |]) ]
	[''Show, ''Storable]

mbType2 :: #{type VkStructureType}
mbType2 = #{const VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2}

struct "MemoryBarrier2" #{size VkBufferMemoryBarrier2}
	#{alignment VkBufferMemoryBarrier2} [
	("sType", ''(), [| const $ pure() |],
		[| \p _ -> #{poke VkBufferMemoryBarrier2, sType} p mbType2 |]),
	("pNext", ''PtrVoid,
		[| #{peek VkBufferMemoryBarrier2, pNext} |],
		[| #{poke VkBufferMemoryBarrier2, pNext} |]),
	("srcStageMask", ''#{type VkPipelineStageFlags2},
		[| #{peek VkBufferMemoryBarrier2, srcStageMask} |],
		[| #{poke VkBufferMemoryBarrier2, srcStageMask} |]),
	("srcAccessMask", ''#{type VkAccessFlags2},
		[| #{peek VkBufferMemoryBarrier2, srcAccessMask} |],
		[| #{poke VkBufferMemoryBarrier2, srcAccessMask} |]),
	("dstStageMask", ''#{type VkPipelineStageFlags2},
		[| #{peek VkBufferMemoryBarrier2, dstStageMask} |],
		[| #{poke VkBufferMemoryBarrier2, dstStageMask} |]),
	("dstAccessMask", ''#{type VkAccessFlags2},
		[| #{peek VkBufferMemoryBarrier2, dstAccessMask} |],
		[| #{poke VkBufferMemoryBarrier2, dstAccessMask} |]),
	("srcQueueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkBufferMemoryBarrier2, srcQueueFamilyIndex} |],
		[| #{poke VkBufferMemoryBarrier2, srcQueueFamilyIndex} |]),
	("dstQueueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkBufferMemoryBarrier2, dstQueueFamilyIndex} |],
		[| #{poke VkBufferMemoryBarrier2, dstQueueFamilyIndex} |]),
	("buffer", ''B,
		[| #{peek VkBufferMemoryBarrier2, buffer} |],
		[| #{poke VkBufferMemoryBarrier2, buffer} |]),
	("offset", ''#{type VkDeviceSize},
		[| #{peek VkBufferMemoryBarrier2, offset} |],
		[| #{poke VkBufferMemoryBarrier2, offset} |]),
	("size", ''#{type VkDeviceSize},
		[| #{peek VkBufferMemoryBarrier2, size} |],
		[| #{poke VkBufferMemoryBarrier2, size} |]) ]
	[''Show, ''Storable]

type PtrMemoryBarrier2 = Ptr MemoryBarrier2

struct "ImageCopy" #{size VkBufferImageCopy} #{alignment VkBufferImageCopy} [
	("bufferOffset", ''#{type VkDeviceSize},
		[| #{peek VkBufferImageCopy, bufferOffset} |],
		[| #{poke VkBufferImageCopy, bufferOffset} |]),
	("bufferRowLength", ''#{type uint32_t},
		[| #{peek VkBufferImageCopy, bufferRowLength} |],
		[| #{poke VkBufferImageCopy, bufferRowLength} |]),
	("bufferImageHeight", ''#{type uint32_t},
		[| #{peek VkBufferImageCopy, bufferImageHeight} |],
		[| #{poke VkBufferImageCopy, bufferImageHeight} |]),
	("imageSubresource", ''Image.SubresourceLayers,
		[| #{peek VkBufferImageCopy, imageSubresource} |],
		[| #{poke VkBufferImageCopy, imageSubresource} |]),
	("imageOffset", ''Offset3d,
		[| #{peek VkBufferImageCopy, imageOffset} |],
		[| #{poke VkBufferImageCopy, imageOffset} |]),
	("imageExtent", ''Extent3d,
		[| #{peek VkBufferImageCopy, imageExtent} |],
		[| #{poke VkBufferImageCopy, imageExtent} |]) ]
	[''Show, ''Storable]
